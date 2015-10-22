package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core._

import scala.io.Source

/**
 * Arduino Code generator
 * Created by Cyril Cecchinel - I3S Laboratory on 02/10/15.
 */

object ArduinoGenerator extends CodeGenerator{


  val LAST_VALUE_PREFIX = "lastValue_"
  val LAST_UPDATE_PREFIX = "lastUpdate_"
  val CURRENT_TIMESTAMP_METHOD:String = "currentTime()"
  val templateFile: String = "assets/generation/arduino/main.template"

  def generatePolicyBody(policy: Policy) = generateInstructionList(policy).foldLeft(""){(acc, e) => acc + e.body + "\n"}
  
  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generateInstruction(_,p)} map {i => i.copy(body = i.body + (if (i.outputs.nonEmpty) " update();" else ""))} map {i => i.copy(body = if (i.inputs.nonEmpty) "if (" + i.inputs.map{_.name + ".t != 0"}.mkString(" && ") + ") {" + i.body + "}" else i.body)}
  }

  def generateInstruction[T<:SensorDataType](c:Concept, policy: Policy):Instruction = c match {

    case a:DataInput[T] =>
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.dataType))
      Instruction(Set(), output_var.name + " = " + LAST_VALUE_PREFIX + a.id + ";", Set(output_var))

    case a:Arithmetic[T] => generateArithmeticInstruction(a)


    case a:Conditional[T] => generateConditionalInstruction(a)


    case a:DataOutput[T] =>
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.dataType))
      Instruction(Set(input_var), "send(" + input_var.name +");", Set())

    case a:Extract[T, T] =>
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))
      Instruction(Set(input_var), output_var.name + " = " + input_var.name + ".data." + a.field + ";", Set(output_var))

    case _ => throw new Exception("Can't generate concept " + c + " for Arduino platform")
  }

  def generateArithmeticInstruction(a: Arithmetic[_ <: SensorDataType]): Instruction = {

    // An arithmetic operation is performed on the Observation Field
    val operationFieldName = DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n

    val operationBody = a match {

      case _:Add[_] => a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("+")
      case _:Sub[_] => a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("-")
      case _:Average[_] => "(" + a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("+") + ")/" + a.inputsNames.size
      case inc:Increment[_,_] => a.id + "_" + inc.input.name + ".data." + operationFieldName + " + " + DataType.getValue(inc.value.asInstanceOf[AtomicType])
      case div:Divide[_,_] => a.id + "_" + div.input.name + ".data." + operationFieldName + " / " + DataType.getValue(div.value.asInstanceOf[AtomicType])
    }


    val operation = "{\""+ a.rename.getOrElse("OP_RESULT_" + a.id)+"\"," + operationBody + ", " + CURRENT_TIMESTAMP_METHOD + "}"


    val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}
    val outputVariables = Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType)))


    MessageBuilder(Instruction(inputVariables, operation, outputVariables))
  }

  def generateConditionalInstruction(a: Conditional[_ <: SensorDataType]): Instruction = {
    //TODO Handle predicate in conditional operations
    val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
    val then_var = Variable(a.id + "_" + a.thenOutput.name, generateDataTypeName(a.oType))
    val else_var = Variable(a.id + "_" + a.elseOutput.name, generateDataTypeName(a.oType))

    Instruction(Set(input_var), "if (" + a.predicate + ") " + then_var.name + " = " + input_var.name + "; " +
      "else " + else_var.name + " = " + input_var.name + ";", Set(then_var, else_var))
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

  def produceSourceFile(name:String, code:String) = {
    super.produceSourceFile(name + "_" + System.currentTimeMillis() , "arduino", "ino", code)
  }

  def apply(p:Policy, toFile:Boolean = false) = {
    if (toFile) produceSourceFile(p.name, generate(p)) else generate(p)
  }

  override def generate(p:Policy) = {
    var generatedCode = super.generate(p)

    generatedCode = replace("update", generateUpdateMethod(p), generatedCode)
    generatedCode = replace("global_sensor_values", generateSensorValues(p), generatedCode)
    generatedCode = replace("setup_instructions", generateSetupInstructions(p), generatedCode)
    generatedCode = replace("period", if (p.hasPeriodicSensors) computePeriod(p).toString else "", generatedCode)
    generatedCode
  }

  def generateSetupInstructions(p:Policy) = {
    try {
      val period = computePeriod(p)
      "TimedEvent.addTimer("+ period * 1000 + ", program_call);"
    } catch {
      case e:Exception => ""

    }
  }
  def generateUpdateMethod(policy: Policy) = {
    "void update() { \n" +
    policy.links.foldLeft(""){(acc, e) => acc + e.destination.id + "_" + e.destination_input.name + " = " + e.source.id + "_" + e.source_output.name + ";\n"} + "}"
  }

  def generateSensorValues(p:Policy) = {
    p.sources.foldLeft(""){(acc, e) => acc + generateDataTypeName(e.dataType) + " " + LAST_VALUE_PREFIX + e.id + ";\n" + "long " + LAST_UPDATE_PREFIX + e.id + ";\n"}
  }

  private def generateDataTypeName[T<:DataType](d:Class[T]) = {
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

  def generateVariablesDeclaration(variables:Set[Variable]) = variables.foldLeft(""){(acc, e) => acc + e.t + " " + e.name + ";\n"}

  override def generateInputs(policy: Policy): (String, String) = {
    def generatePeriodicDeclaration(a:PeriodicSensor[_], f:String) = "TimedEvent.addTimer(" + a.wishedPeriod * 1000 + ", " + f + ");\n"
    def generateEventDeclaration(a:EventSensor[_], f:String) = "AnalogEvent.addAnalogPort(" + Utils.lookupSensorAssignment(a.name) + ", " + f + ", 500);\n"
    var body = ""
    var declaration = ""
    for (s <- policy.sources) {
      s match {
        case a:EventSensor[_] =>
          val res = generateEventSensor(a);
          declaration = declaration + generateEventDeclaration(a, res._1);
          body = body + res._2
        case a:PeriodicSensor[_] =>
          val res = generatePeriodicSensor(a);
          declaration = declaration + generatePeriodicDeclaration(a, res._1);
          body = body + res._2
      }
    }
    (declaration, body)
  }


  private def generateEventSensor(s:EventSensor[_<:DataType]) = {
    val template = "assets/generation/arduino/eventsensor.template"
    val name = "event_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("id", s.id, body)
    body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    body = replace("common_name", s.name, body)
    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body + "\n", vars)

  }

  private def generatePeriodicSensor(s:PeriodicSensor[_<:DataType]) = {
    val template = "assets/generation/arduino/periodicsensor.template"
    val name = "periodic_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("id", s.id, body)
    body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    body = replace("common_name", s.name, body)

    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body + "\n", vars)

  }


  object MessageBuilder {
    def apply(instruction: Instruction) = generate(instruction)
    def generate(instruction: Instruction) = Instruction(instruction.inputs,instruction.outputs.head.name +  " = {" + CURRENT_TIMESTAMP_METHOD + ", BOARD_ID," + instruction.body + "};", instruction.outputs)
  }
}
