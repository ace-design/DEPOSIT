package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core._

import scala.io.Source

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 02/10/15.
 */

object ArduinoGenerator extends CodeGenerator{


  val LAST_VALUE_PREFIX = "lastValue_"
  val LAST_UPDATE_PREFIX = "lastUpdate_"
  val CURRENT_TIMESTAMP_METHOD:String = "currentTime()"
  val templateFile: String = "assets/generation/arduino/main.template"

  object MessageBuilder {
    def apply(instruction: Instruction) = generate(instruction)
    def generate(instruction: Instruction) = Instruction(instruction.inputs,instruction.outputs.head.name +  " = {" + CURRENT_TIMESTAMP_METHOD + ", BOARD_ID," + instruction.body + "};", instruction.outputs)
  }
  
  def generatePolicyBody(policy: Policy) = generateInstructionList(policy).foldLeft(""){(acc, e) => acc + e.body + "\n"}

  def generateArithmeticInstruction(a: Arithmetic[_ <: SensorDataType]): Instruction = {
    // An arithmetic operation is performed on the Observation Field
    val operationFieldName = DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n
    val operation = "{\"name\"," + a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("+") + ", " + CURRENT_TIMESTAMP_METHOD + "}"


    val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}
    val outputVariables = Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType)))


    MessageBuilder(Instruction(inputVariables, operation, outputVariables))
  }


  def generateConditionalInstruction(a: Conditional[_ <: SensorDataType]): Instruction = {
    val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
    val then_var = Variable(a.id + "_" + a.thenOutput.name, generateDataTypeName(a.oType))
    val else_var = Variable(a.id + "_" + a.elseOutput.name, generateDataTypeName(a.oType))

    Instruction(Set(input_var), "if (" + a.predicate + ") " + then_var.name + " = " + input_var.name + "; " +
      "else " + else_var.name + " = " + input_var.name + ";", Set(then_var, else_var))
  }

  def generateInstruction[T<:SensorDataType](c:Concept, policy: Policy):Instruction = c match {

    case a:DataInput[T] => {
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.dataType))
      Instruction(Set(), output_var.name + " = " + LAST_VALUE_PREFIX + a.id + ";", Set(output_var))
    }

    //TODO Arithmetic operation
    case a:Arithmetic[T] => generateArithmeticInstruction(a)
    //TODO Handle predicate in conditional operations


    case a:Conditional[T] => generateConditionalInstruction(a)


    case a:DataOutput[T] => {
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.dataType))
      Instruction(Set(input_var), "send(" + input_var.name + "," + a.name + ");", Set())
    }

    case a:Extract[T, T] => {
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))
      Instruction(Set(input_var), output_var.name + " = " + input_var.name + ".data." + a.field + ";", Set(output_var))
    }

    case _ => throw new Exception("Can't generate concept " + c + " for Arduino platform")
  }



  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generateInstruction(_,p)} map {i => i.copy(body = i.body + (if (i.outputs.nonEmpty) " update();" else ""))}
  }

  def generateVariablesDeclaration(variables:Set[Variable]) = variables.foldLeft(""){(acc, e) => acc + e.t + " " + e.name + ";\n"}



  def generateUpdateMethod(policy: Policy) = {
    "void update() { \n" +
    policy.links.foldLeft(""){(acc, e) => acc + e.destination.id + "_" + e.destination_input.name + " = " + e.source.id + "_" + e.source_output.name + ";\n"} + "}"
  }

  //TODO: Passer en private def
  //TODO: refactoring (code duplication with generatePeriodicSensor)
  def generateEventSensor(s:EventSensor[_<:SensorDataType]) = {
    val template = "assets/generation/arduino/eventsensor.template"
    val name = "event_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("id", s.id, body)
    body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    body = replace("common_name", s.name, body)
    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body, vars)

  }
  //TODO: Passer en private def

  def generatePeriodicSensor(s:PeriodicSensor[_<:SensorDataType]) = {
    val template = "assets/generation/arduino/periodicsensor.template"
    val name = "periodic_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("id", s.id, body)
    body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    body = replace("common_name", s.name, body)

    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body, vars)

  }


  def generateDataStructures(p:Policy) = {
    def generateStruct(dType:DataType) = {
      def _innerstruct(c:CompositeType) = {
        "struct inner_" + c.name + "{\n" +
          c.bindings.foldLeft(""){(acc, e) => acc + generateDataTypeName(e._2) + " " + e._1 + ";\n"} + "};\n"
      }

      (dType match {
        case c:CompositeType => _innerstruct(c)
        case _ => ""
      }) + "struct " + dType.name + "{\n" + "long t;\nint src;\n" + (
        dType match {
          case a:AtomicType => generateDataTypeName(dType.getClass)
          case a:CompositeType => "struct inner_" + dType.name
        }
        )  + " data;\n};\n"

    }

    p.dataTypesInvolved.foldLeft(""){(acc, e) => acc + generateStruct(DataType.factory(e.getSimpleName)) + "\n"}
  }

  override def generate(p:Policy) = {
    var generatedCode = super.generate(p)

    generatedCode = replace("update", generateUpdateMethod(p), generatedCode)
    generatedCode = replace("global_sensor_values", generateSensorValues(p), generatedCode)
    generatedCode
  }

  def generateSensorValues(p:Policy) = {
    p.sources.foldLeft(""){(acc, e) => acc + generateDataTypeName(e.dataType) + " " + LAST_VALUE_PREFIX + e.id + ";\n" + "long " + LAST_UPDATE_PREFIX + e.id + ";\n"}
  }
  def generateDataTypeName[T<:DataType](d:Class[T]) = {
    d.getSimpleName match {
      case "IntegerType" => "int"
      case "LongType" => "long"
      case "StringType" => "String"
      case "DoubleType" => "double"
      case "SmartCampusType" => "struct SmartCampusType"
      case "SantanderParkingType" => "struct SantanderParkingType"
      case "IntegerSensorType" => "struct IntegerSensorType"
      case _ => throw new Exception("Unknown data type name")
    }
  }

  /**
   * Convert sensor ID for Arduino
   * @param id Raw ID
   * @return Arduino ID
   */
  def convertId(id: String): Int = {
    try {
      val x = Integer.parseInt(id)
      if (x > 0) x
      else throw new Exception("Arduino supports only positive id for sensors")
    }
    catch {
      case e: NumberFormatException => throw new Exception("Arduino only support integer id for sensors")
    }
  }

  override def generateGlobalVariables(policy: Policy): String = generateVariablesDeclaration(generateInstructionList(policy).flatMap(i => Set(i.inputs, i.outputs)).flatten.toSet)
}
