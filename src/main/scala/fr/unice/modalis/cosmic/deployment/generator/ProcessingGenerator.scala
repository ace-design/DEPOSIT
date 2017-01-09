package fr.unice.modalis.cosmic.deployment.generator

import com.typesafe.scalalogging.LazyLogging
import fr.unice.modalis.cosmic.deployment.exception.InappropriateConceptForGenerator
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.ProgrammingLanguage.ProgrammingLanguage
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{ProgrammingLanguage, SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deployment.network.Media
import fr.unice.modalis.cosmic.deployment.network.Media.Media
import fr.unice.modalis.cosmic.deposit.core._

import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Arduino Code generator
 * Created by Cyril Cecchinel - I3S Laboratory on 02/10/15.
 */

object ProcessingGenerator extends CodeGenerator with LazyLogging{


  val LAST_VALUE_PREFIX = "lastValue_"
  val LAST_UPDATE_PREFIX = "lastUpdate_"
  val CURRENT_TIMESTAMP_METHOD:String = "currentTime()"
  val templateFile: String = "assets/generation/arduino/main.template"

  def generatePolicyBody(policy: Policy) = generateInstructionList(policy).foldLeft(""){(acc, e) => acc + e.body + "\n"}
  
  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generateInstruction(_,p)} map {i => i.copy(body = i.body + (if (i.outputs.nonEmpty) " update();" else ""))}
  }

  def generateInstruction[T<:SensorDataType, U<:SensorDataType](c:Concept, policy: Policy):Instruction = c match {

    case a:Rename[T] =>
      val input_var = Variable(a.id + "_" + a.input.name, a.iType.getSimpleName)
      val output_var = Variable(a.id + "_" + a.output.name, a.oType.getSimpleName)
      Instruction(
        Set(input_var),
        input_var.name + ".data." + DataType.factory(input_var.t.replace("struct", "").trim).asInstanceOf[SensorDataType].getNameField.n + " = \"" + a.newName + "\";" +
        output_var.name + " = " + input_var.name + ";",
        Set(output_var))

    case a:DataInput[T] =>
      val output_var = Variable(a.id + "_" + a.output.name, a.dataType.getSimpleName)
      Instruction(Set(), output_var.name + " = " + LAST_VALUE_PREFIX + a.id + ";", Set(output_var))

    case a:Arithmetic[T] => generateArithmeticInstruction(a)


    case a:Conditional[T] => generateConditionalInstruction(a)


    case a:Collector[T] =>
      val connection = a.readProperty("connection").asInstanceOf[Option[Media]]
      // If policy has been pre-deployed
      val sendMethod = if (connection.isDefined){
        connection.get match {
          case Media.I2C => {
            if (!(a.dataType.getSimpleName equals "IntegerType")){
              logger.warn("I2C has been implemented only with IntegerType. Code compilation might fail.")
            }
            "sendI2C"
          }
          case _ => "send"
        }
      } else "send"

      val input_var = Variable(a.id + "_" + a.input.name, a.dataType.getSimpleName)
      Instruction(Set(input_var), sendMethod + "(" + input_var.name +");", Set())

    case a:JoinPointOutput[T] =>
      val id = a.readProperty("network")
      val origin = if (id.isDefined) "\"" + id.get + "\"" else "String(BOARD_ID)"
      val input_var = Variable(a.id + "_" + a.input.name, a.dataType.getSimpleName)
      Instruction(Set(input_var), "send(" + input_var.name + "," + origin + ");", Set())

    case a:JoinPointInput[T] =>
      val id = a.readProperty("network")
      val origin = if (id.isDefined) "\"" + id.get + "\"" else "String(BOARD_ID)"
      val output_var = Variable(a.id + "_" + a.output.name, a.dataType.getSimpleName)
      Instruction(Set(), "receive(" + output_var.name + "," + origin + ");", Set(output_var))

    case a:Extract[T, T] =>
      val input_var = Variable(a.id + "_" + a.input.name, a.iType.getSimpleName)
      val output_var = Variable(a.id + "_" + a.output.name, a.oType.getSimpleName)
      MessageBuilder.generate(Instruction(Set(input_var), input_var.name + ".data." + a.field, Set(output_var)))


    case a:Produce[T,U] =>
      val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, a.iType.getSimpleName)}
      val outputVariable = Variable(a.id + "_" + a.output.name, a.oType.getSimpleName)
      val bodyInstruction =
        "if (" + a.inputsNames.map(i => a.id + "_" + i + ".t != -1").mkString(" && ") + ")" + MessageBuilder(Instruction(Set(),generateConstant(a.onSuccess),Set(outputVariable))).body +
          (if (a.onFailure.isDefined) " else " + MessageBuilder(Instruction(Set(),generateConstant(a.onFailure.get),Set(outputVariable))).body else "")

      Instruction(inputVariables, bodyInstruction, Set(outputVariable))


    case _ => throw new InappropriateConceptForGenerator(c, "Arduino")
  }

  def generateConstant(d:DataType) = d match {
    case s:SensorDataType => "{" + (for (e <- s.value.productIterator) yield DataType.getValue(e.asInstanceOf[AtomicType])).mkString(",") + "}"
    case a:AtomicType => DataType.getValue(a).toString
  }

  def generateArithmeticInstruction(a: Arithmetic[_ <: SensorDataType]): Instruction = {

    // An arithmetic operation is performed on the Observation Field
    val operationFieldName = DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n

    val operationBody = a match {

      case _:Add[_] => a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("+")
      case _:Sub[_] => a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("-")
      case _:Average[_] => "(" + a.inputsNames.map(i => a.id + "_" + i + ".data." + operationFieldName).mkString("+") + ")/" + a.inputsNames.size
      case inc:Increment[_,_] => a.id + "_" + inc.input.name + ".data." + operationFieldName + " + " + DataType.getValue(inc.value.asInstanceOf[AtomicType])
      case mul:Multiply[_,_] => a.id + "_" + mul.input.name + ".data." + operationFieldName + " * " + DataType.getValue(mul.value.asInstanceOf[AtomicType])
      case div:Divide[_,_] => a.id + "_" + div.input.name + ".data." + operationFieldName + " / " + DataType.getValue(div.value.asInstanceOf[AtomicType])
    }


    val operation = "{\""+ a.rename.getOrElse("OP_RESULT_" + a.id)+"\"," + operationBody + ", " + CURRENT_TIMESTAMP_METHOD + "}"


    val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, a.iType.getSimpleName)}
    val outputVariables = Set(Variable(a.id + "_" + a.output.name, a.oType.getSimpleName))


    MessageBuilder(Instruction(inputVariables, operation, outputVariables))
  }

  def generateConditionalInstruction(a: Conditional[_ <: SensorDataType]): Instruction = {
    //TODO Handle predicate in conditional operations
    val input_var = Variable(a.id + "_" + a.input.name, a.iType.getSimpleName)
    val then_var = Variable(a.id + "_" + a.thenOutput.name, a.oType.getSimpleName)
    val else_var = Variable(a.id + "_" + a.elseOutput.name, a.oType.getSimpleName)

    val predicate = a.predicate.replace("value", input_var.name + ".data." + DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n)
    Instruction(Set(input_var), "if (" + predicate + ") {" + then_var.name + " = " + input_var.name + "; " + else_var.name + " = nullValue_" + else_var.t + ";} " +
      "else {" + else_var.name + " = " + input_var.name + ";" + then_var.name + " = nullValue_"+ then_var.t +";}", Set(then_var, else_var))
  }

  def produceSourceFile(name:String, code:String) = {
    CodeGenerator.produceSourceFile(name + "_" + System.currentTimeMillis() , "arduino", "ino", code)
  }



  override def generate(p:Policy) = {
    var generatedCode = super.generate(p)

    generatedCode = replace("update", generateUpdateMethod(p), generatedCode)
    generatedCode = replace("global_sensor_values", generateSensorValues(p), generatedCode)
    generatedCode = replace("setup_instructions", generateSetupInstructions(p), generatedCode)
    generatedCode = replace ("global_pointers", generateGlobalPointers(p), generatedCode)
    generatedCode = replace ("libraries", generateLibraries(p), generatedCode)
    generatedCode = replace("period", if (hasPeriodicSensors(p)) computePeriod(p).toString else "", generatedCode)
    generatedCode = replace("board_type", generateBoardType(p), generatedCode)
    generatedCode
  }

  def generateBoardType(p:Policy) = {
    p.readProperty("board_type").get.asInstanceOf[String] match {
      case "Yun" => "0"
      case _ => "1" //Default: Uno platform
    }
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
    policy.flows.foldLeft(""){ (acc, e) => acc + e.destination.id + "_" + e.destination_input.name + " = " + e.source.id + "_" + e.source_output.name + ";\n"} + "}"
  }

  def generateFlushMethod(policy: Policy) = {
    "void flush() {\n" +
     policy.flows.foldLeft(""){ (acc, e) => acc + e.source.id + "_" + e.source_output.name + " = nullValue_" + e.source_output.dType.getSimpleName + ";\n"} + "}"
  }

  def generateSensorValues(p:Policy) = {
    p.sources.foldLeft(""){(acc, e) => acc + generateConcreteDataTypeName(e.dataType) + " " + LAST_VALUE_PREFIX + e.id + ";\n" + "long " + LAST_UPDATE_PREFIX + e.id + ";\n"}
  }

  private def generateConcreteDataTypeName[T<:DataType](d:Class[T]) = {
    d.getSimpleName match {
      case "IntegerType" => "int"
      case "LongType" => "long"
      case "StringType" => "String"
      case "DoubleType" => "double"
      case "SmartCampusType" => "SmartCampusType"
      case "SantanderParkingType" => "SantanderParkingType"
      case "IntegerSensorType" => "IntegerSensorType"
      case _ => throw new UnknownDataTypeName(d.getSimpleName)
    }
  }

  /**
   * Convert sensor ID for Arduino
 *
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

  override def generateGlobalVariables(policy: Policy): String = {
    generateVariablesDeclaration(generateInstructionList(policy).flatMap(i => Set(i.inputs, i.outputs)).flatten.toSet)
  }

  def generateVariablesDeclaration(variables:Set[Variable]) = variables.foldLeft(""){(acc, e) => acc + e.t + " " + e.name + ";\n"}

  override def generateInputs(policy: Policy): (String, String) = {
    def generatePeriodicDeclaration(a:PeriodicSensor[_], f:String) = "TimedEvent.addTimer(" + a.wishedPeriod * 1000 + ", " + f + ");\n"
    def generateEventDeclaration(a:EventSensor[_], f:String) = {
      val pin = if (a.hasProperty("pin").isDefined) a.readProperty("pin").get.asInstanceOf[String] else Utils.lookupSensorAssignment(a.name)
      "AnalogEvent.addAnalogPort(" +  pin + ", " + f + ", 500);\n"
    }

    var body = ""
    var declaration = ""
    for (s <- policy.sources) {
      s match {
        case a:EventSensor[_] =>
          val res = generateEventSensor(a)
          declaration = declaration + generateEventDeclaration(a, res._1)
          body = body + res._2
        case a:PeriodicSensor[_] =>
          val res = generatePeriodicSensor(a)
          declaration = declaration + generatePeriodicDeclaration(a, res._1)
          body = body + res._2
        case a:JoinPointInput[_] =>
         logger.warn("Join point inputs are MOCKED on ARDUINO")
        case _ => throw new InappropriateConceptForGenerator(s, "Arduino")
      }
    }
    (declaration, body)
  }


  private def generateEventSensor(s:EventSensor[_<:DataType]) = {
    val template = "assets/generation/arduino/eventsensor.template"
    val name = "event_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("call_method", sensorTypeHandling(s.readProperty("type").get.asInstanceOf[SensorType.Value])._1, body)
    body = replace("id", s.id, body)
    if (s.hasProperty("pin").isDefined) {
      body = replace("port", s.readProperty("pin").get.asInstanceOf[String], body)
    } else {
      body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    }
    body = replace("common_name", s.name, body)
    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateConcreteDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body + "\n", vars)

  }

  private def generatePeriodicSensor(s:PeriodicSensor[_<:DataType]) = {
    val template = "assets/generation/arduino/periodicsensor.template"
    val name = "periodic_" + s.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("call_method", sensorTypeHandling(s.readProperty("type").get.asInstanceOf[SensorType.Value])._1, body)
    body = replace("id", s.id, body)
    if (s.hasProperty("pin").isDefined) {
      body = replace("port", s.readProperty("pin").get.asInstanceOf[String], body)
    } else {
      body = replace("port", Utils.lookupSensorAssignment(s.name), body)
    }
    body = replace("common_name", s.name, body)

    val vars = Set(Variable(LAST_VALUE_PREFIX + s.id, generateConcreteDataTypeName(s.dataType)), Variable("lastUpdate_" + s.id, "long"))
    (name, body + "\n", vars)

  }

  private def generateGlobalPointers(p:Policy) = {
    p.inputs.collect{case x:Sensor[_] => x}.foldLeft("") {
      (acc, e) => {
        val parent = sensorTypeHandling(e.readProperty("type").get.asInstanceOf[SensorType.Value])._3
        val brand = sensorBrandHandling(e.readProperty("brand").get.asInstanceOf[SensorBrand.Value])._2
        val pin = if (e.hasProperty("pin").isDefined) e.readProperty("pin").get else Utils.lookupSensorAssignment(e.name)
        acc + parent + " *" + e.id + " = new " + brand + "(" + pin + ");\n"
      }}
  }

  private def generateLibraries(p:Policy) = {
    p.inputs.collect{case x:Sensor[_] => x}.foldLeft("") {
      (acc, e) => {
        val brand = sensorBrandHandling(e.readProperty("brand").get.asInstanceOf[SensorBrand.Value])._1
        acc + "#include <" + brand + ">\n"
      }}
  }


  object MessageBuilder {
    def apply(instruction: Instruction) = generate(instruction)
    def generate(instruction: Instruction) = Instruction(instruction.inputs,instruction.outputs.head.name +  " = {" + CURRENT_TIMESTAMP_METHOD + ", BOARD_ID," + instruction.body + "};", instruction.outputs)
  }

  override val language: ProgrammingLanguage = ProgrammingLanguage.Processing
  override val sensorTypeHandling: HashMap[SensorType.Value, (String, String, String)] = HashMap(
    SensorType.Temperature -> ("readTemperature()",generateConcreteDataTypeName(classOf[DoubleType]), "TemperatureSensor"),
    SensorType.Magnetic -> ("readValue()", generateConcreteDataTypeName(classOf[IntegerType]), "RawSensor"),
    SensorType.Sound -> ("readValue()", generateConcreteDataTypeName(classOf[IntegerType]), "RawSensor")
  )

  override val sensorBrandHandling: HashMap[SensorBrand.Value, (String, String)] = HashMap(
    SensorBrand.GroveTemperature -> ("grovetemperature.h", "GroveTemperatureSensor"),
    SensorBrand.EBTemperature -> ("ebtemperature.h", "EBTemperatureSensor"),
    SensorBrand.DFTemperature -> ("dftemperature.h", "DFTemperatureSensor"),
    SensorBrand.GroveMagnetic -> ("raw.h", "RawSensor"),
    SensorBrand.GroveSound -> ("raw.h", "RawSensor")
  )

  override def generateIntraMessage(dataType: DataType): String = s"struct ${dataType.name}{long t; String src; struct inner_${dataType.name} data;};"

  override def generateDataStructure(dataType: DataType): String = {
    s"struct inner_${dataType.name}{" + (dataType match {
      case c:CompositeType =>c.bindings.foldLeft(""){(acc, e) => acc + s"${generateConcreteDataTypeName(e._2)} ${e._1};"}
      case a:AtomicType => generateConcreteDataTypeName(dataType.getClass) + " v;"
      case _ => ""
    }) + "};\n" + generateIntraMessage(dataType) + "\n" + generateNullValue(dataType) + "\n"
  }

  def generateNullValue(dataType: DataType):String = {
    s"struct ${dataType.name} nullValue_${dataType.name} = {-1, BOARD_ID, " + (dataType match {
      case d:SensorDataType => generateConstant(DataType.nullValue(dataType.name))
      case a:AtomicType => "{" + DataType.getValue(DataType.nullValue(dataType.name).asInstanceOf[AtomicType]) + "}"
    }) + "};"
  }


  /**
    * Generate associated Data Structures
    *
    * @param p Data collection policy
    * @return Compilable code defining data structures
    */
  override def generateDataStructures(p: Policy): String = {p.getInvolvedDataTypes.foldLeft(""){(acc, e) => acc + generateDataStructure(DataType.factory(e.getSimpleName))}}

}
