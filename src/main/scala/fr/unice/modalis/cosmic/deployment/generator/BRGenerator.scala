package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core._

import scala.io.Source

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 29/10/2015.
 */
object BRGenerator extends CodeGenerator{
  override val templateFile: String = "assets/generation/python/main.template"
  override val CURRENT_TIMESTAMP_METHOD: String = "currentTime()"

  val LAST_VALUE_PREFIX = "lastValue_"
  val LAST_UPDATE_PREFIX = "lastUpdate_"

  def generateReadingPorts(policy: Policy) = {
    policy.inputJoinPoints.map(i => i.id + "_" + i.output.name + "_PORT = #@fill port here@#\n").mkString
  }

  def generateSerialReadingMethodForJoinPoints(policy: Policy) = {
    "def readSerial(port):\n" +
      policy.inputJoinPoints.map(i => "\tglobal " + LAST_VALUE_PREFIX + i.id + "\n").mkString +
      policy.inputJoinPoints.map(i => "\t" +LAST_VALUE_PREFIX + i.id + "= []\n").mkString +
      "\tflush()\n" +
      "\twhile True:\n" +
      "\t\tser = serial.Serial(port, 9600)\n" +
      "\t\ttry:\n" +
      "\t\t\tvalueRead = json.loads(ser.readline().decode(\"ascii\"))\n" +
      policy.inputJoinPoints.map(i => {
        val network = i.readProperty("network").get
        "\t\t\tif valueRead[\"src\"] == \"" +  network + "\":\n" +
        "\t\t\t\t" + LAST_VALUE_PREFIX + i.id + " = valueRead\n"
      }).mkString +
      "\t\t\tprogram()\n" +
      "\t\texcept Exception:\n\t\t\tpass"


  }
  /**
   * Building an instruction from a concept
   * @param c Concept
   * @param policy Data collection policy
   * @tparam T Input sensor data type
   * @tparam U Output sensor data type
   * @return An instruction
   */
  override def generateInstruction[T <: SensorDataType, U <: SensorDataType](c: Concept, policy: Policy): Instruction = c match {
    /*case a:DataInput[T] =>
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.dataType))
      Instruction(Set(), output_var.name + " = " + LAST_VALUE_PREFIX + a.id, Set(output_var))
*/

    case a:Rename[T] =>
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))
      Instruction(
        Set(input_var),
        input_var.name + "[\"data\"][\"" + DataType.factory(input_var.t).asInstanceOf[SensorDataType].getNameField.n + "\"] = \"" + a.newName + "\";" +
          output_var.name + " = " + input_var.name,
        Set(output_var))

    case a:Arithmetic[T] => generateArithmeticInstruction(a)

    case a:Conditional[T] => generateConditionalInstruction(a)

    case a:Collector[T] =>
      val input_var = Variable(a.id + "_" + a.input.name, "")
      Instruction(Set(input_var), "send(" + input_var.name +")", Set())

    case a:JoinPointOutput[T] =>
      val id = a.readProperty("network")
      val origin = if (id.isDefined) "\"" + id.get + "\"" else "String(BOARD_ID)"
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.dataType))
      Instruction(Set(input_var), "send(" + input_var.name + "," + origin + ");", Set())

    case a:JoinPointInput[T] if a.hasProperty("network").isDefined =>
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.dataType))
      Instruction(Set(), output_var.name + " = " + LAST_VALUE_PREFIX + a.id, Set(output_var))


    case a:Extract[T, T] =>
      val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
      val output_var = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))
      Instruction(Set(input_var), output_var.name + " = " + input_var.name + ".data." + a.field + ";", Set(output_var))


    case a:Produce[T,U] =>
      val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}
      val outputVariable = Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))
      val bodyInstruction =
        "if " + a.inputsNames.map(i => a.id + "_" + i + "[\"t\"] != -1").mkString(" and ") + ":\n\t\t" + MessageBuilder(Instruction(Set(),generateConstant(a.onSuccess),Set(outputVariable))).body +
          (if (a.onFailure.isDefined) "\n\telse:\n\t\t" + MessageBuilder(Instruction(Set(),generateConstant(a.onFailure.get),Set(outputVariable))).body else "")

      Instruction(inputVariables, bodyInstruction, Set(outputVariable))


    case _ => throw new Exception("Can't generate concept " + c + " for Python platform")
  }

  def generateArithmeticInstruction(a: Arithmetic[_ <: SensorDataType]): Instruction = {

    // An arithmetic operation is performed on the Observation Field
    val operationFieldName = DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n

    val operationBody = a match {

      case _:Add[_] => a.inputsNames.map(i => a.id + "_" + i + "[\"data\"][\"" + operationFieldName + "\"]").mkString("+")
      case _:Sub[_] => a.inputsNames.map(i => a.id + "_" + i + "[\"data\"][\"" + operationFieldName + "\"]").mkString("-")
      case _:Average[_] => "(" + a.inputsNames.map(i => a.id + "_" + i + "[\"data\"][\"" + operationFieldName + "\"]").mkString("+") + ")/" + a.inputsNames.size
      case inc:Increment[_,_] => a.id + "_" + inc.input.name + "[\"data\"][\"" + operationFieldName + "\"]" + " + " + DataType.getValue(inc.value.asInstanceOf[AtomicType])
      case mul:Multiply[_,_] => a.id + "_" + mul.input.name + "[\"data\"][\"" + operationFieldName + "\"]" + " * " + DataType.getValue(mul.value.asInstanceOf[AtomicType])
      case div:Divide[_,_] => a.id + "_" + div.input.name + "[\"data\"][\"" + operationFieldName + "\"]" + " / " + DataType.getValue(div.value.asInstanceOf[AtomicType])
    }


    val operation = "{\"n\":\""+ a.rename.getOrElse("OP_RESULT_" + a.id)+"\",\"v\":" + operationBody + ",\"t\":" + CURRENT_TIMESTAMP_METHOD + "}"


    val inputVariables = a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}
    val outputVariables = Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType)))


    MessageBuilder(Instruction(inputVariables, operation, outputVariables))
  }

  def generateConditionalInstruction(a: Conditional[_ <: SensorDataType]): Instruction = {
    //TODO Handle predicate in conditional operations
    val input_var = Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))
    val then_var = Variable(a.id + "_" + a.thenOutput.name, generateDataTypeName(a.oType))
    val else_var = Variable(a.id + "_" + a.elseOutput.name, generateDataTypeName(a.oType))

    val predicate = a.predicate.replace("value", input_var.name + "[\"data\"][\"" + DataType.factory(a.iType.getSimpleName).asInstanceOf[SensorDataType].getObservationField.n + "\"]")
    Instruction(Set(input_var), "if " + predicate + ":\n\t\t" + then_var.name + " = " + input_var.name + ";" + else_var.name + " = nullValue\n" +
      "\telse:\n\t\t" + else_var.name + " = " + input_var.name + ";" + then_var.name + " = nullValue", Set(then_var, else_var))
  }
  /**
   * Generate the data inputs
   * @param policy Data collection policy
   * @return Compilable code defining data inputs
   */
  override def generateInputs(policy: Policy): (String, String) = {
    def generateJoinPointDeclaration(a:JoinPoint[_], f:String) = "joinpoint_" + a.id + "\n"
    var body = ""
    var declaration = ""
    for (s <- policy.inputs) {
      s match {
        case a:JoinPointInput[_] =>
          val res = generateJoinPointInput(a)
          declaration = declaration + generateJoinPointDeclaration(a, res._1)
          body = body + res._2
      }
    }

    (declaration, body)
  }

  /**
   * Generate global variables
   * @param policy Data collection policy
   * @return Compilable code defining global variables
   */
  override def generateGlobalVariables(policy: Policy): String = generateGlobalVariables(policy, inBody = false)

  def generateGlobalVariables(policy: Policy, inBody:Boolean = false): String = if (inBody) generateVariablesDeclaration(generateInstructionList(policy).flatMap(i => Set(i.inputs, i.outputs)).flatten.toSet, inBody = true) else generateVariablesDeclaration(generateInstructionList(policy).flatMap(i => Set(i.inputs, i.outputs)).flatten.toSet)
  /**
   * Generate the executable data collection policy
   * @param policy Data collection policy
   * @return Compilable code defining the policy body
   */
  override def generatePolicyBody(policy: Policy) = generateInstructionList(policy).foldLeft(""){(acc, e) => acc + "\t" + e.body + "\n"}

  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generateInstruction(_,p)} map {i => i.copy(body = i.body + (if (i.outputs.nonEmpty) "; update()" else ""))}
  }

  def generateVariablesDeclaration(variables:Set[Variable], inBody:Boolean = false) = variables.foldLeft(""){(acc, e) => acc + (if (inBody) "\tglobal " else "") + e.name + "\n"}

  def generateUpdateMethod(policy: Policy) = {
    "def update(): \n" +
      generateGlobalVariables(policy, inBody = true) + "\n" +
      policy.links.foldLeft(""){(acc, e) => acc + "\t" + e.destination.id + "_" + e.destination_input.name + " = " + e.source.id + "_" + e.source_output.name + ";\n"}
  }

  def generateFlushMethod(policy: Policy) = {
    "def flush(): \n" +
      policy.links.foldLeft(""){(acc, e) => acc + "\tglobal " + e.source.id + "_" + e.source_output.name + "\n"} +
      policy.links.foldLeft(""){(acc, e) => acc + "\t" + e.source.id + "_" + e.source_output.name + " = []\n"}
  }
  /**
   * Generate constant
   * @param s Sensor data value
   * @return Compilable code defining a constant
   */
  override def generateConstant(s:SensorDataType) = {
    s match {
      case SmartCampusType((n, v, t)) if t.value == 0 => "{\"n\":\"" + n.value + "\",\"v\":\"" + v.value + "\", \"t\": currentTime()}"
      case SmartCampusType((n, v, t)) => "{\"n\":\"" + n.value + "\",\"v\":" + v.value + ", \"t\":" + t.value + "}"
    }
  }

  /**
   * Generate associated Data Structures
   * @param p Data collection policy
   * @return Compilable code defining data structures
   */
  override def generateDataStructures(p: Policy): String = ""

  /**
   * Produce a compilable source file
   * @param name File name
   * @param code Compilable code
   */
  override def produceSourceFile(name: String, code: String): Unit = {
    CodeGenerator.produceSourceFile(name + "_" + System.currentTimeMillis() , "python", "py", code)
  }


  private def generateDataTypeName[T<:DataType](d:Class[T]) = ""

  private def generateJoinPointInput(j:JoinPointInput[_<:DataType]) = {
    val template = "assets/generation/python/readJoinPoint.template"
    val name = "joinpoint_" + j.id
    var body = Source.fromFile(template).getLines().mkString("\n")
    body = replace("name", name, body)
    body = replace("id", j.id + "_" + j.output.name, body)
    val vars = Set(Variable(j.id + "_" + j.output.name, ""))
    (name, body + "\n", vars)
  }



  override def generate(p:Policy) = {
    var generatedCode = super.generate(p)
    generatedCode = replace("serial", generateSerialReadingMethodForJoinPoints(p), generatedCode)
    generatedCode = replace("update", generateUpdateMethod(p), generatedCode)
    generatedCode = replace("flush", generateFlushMethod(p), generatedCode)
    generatedCode = replace("use_global_variables", generateGlobalVariables(p, inBody = true), generatedCode)

    generatedCode
  }

  object MessageBuilder {
    def apply(instruction: Instruction) = generate(instruction)
    def generate(instruction: Instruction) = Instruction(instruction.inputs,instruction.outputs.head.name +  " = {\"t\":" + CURRENT_TIMESTAMP_METHOD + ",\"src\": BOARD_ID, \"data\":" + instruction.body + "}", instruction.outputs)
  }

}
