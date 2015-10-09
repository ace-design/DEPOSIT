package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core._

import scala.io.Source

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 02/10/15.
 */

object ArduinoGenerator extends CodeGenerator{


  val LAST_VALUE_PREFIX = "lastValue_"


  def generate(c:Concept, policy: Policy):Instruction = c match {
    case a:DataInput[_] => Instruction(Set(), generateDataTypeName(a.dataType) + " " + a.id + "_" + a.output.name + " = " + LAST_VALUE_PREFIX + a.id + ";", Set(Variable(a.output.name, generateDataTypeName(a.dataType))))
    //TODO Arithmetic operation
    case a:Arithmetic[_] => Instruction(a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}, "/* TODO Arithmetic operation */", Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))))
    case a:Conditional[_] => Instruction(Set(Variable(c.id + "_" + a.input.name, generateDataTypeName(a.iType))), "if (" + a.predicate + ") " + generateDataTypeName(a.oType) + " " + a.id + "_" + a.thenOutput.name + " = " + c.id + "_" + a.input.name + "; else " + generateDataTypeName(a.oType) + " " +  a.elseOutput.name + " = " + c.id + "_" + a.input.name + ";", Set(Variable(a.id + "_" + a.thenOutput.name, generateDataTypeName(a.oType))))
    case a:DataOutput[_] => Instruction(Set(Variable(a.id + "_" + a.input.name, generateDataTypeName(a.dataType))), "send(" + a.id + "_" + a.input.name + "," + a.name + ");", Set())
    case a:Extract[_, _] => Instruction(Set(Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))), generateDataTypeName(a.oType) + " " + a.id + "_" + a.output.name + " = " + a.id + "_" + a.input.name + ".data." + a.field + ";", Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))))
    case _ => throw new Exception("Can't generate concept " + c + " for Arduino platform")
  }

  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generate(_,p)} map {i => i.copy(body = i.body + (if (i.outputs.nonEmpty) " update();" else ""))}
    //orderedGenerationList(p).map {generate(_,p)} map {i => i.copy(body = i.body + ." update();"))}
  }

  def generateUpdateMethod(policy: Policy) = {
    "void update() { \n" +
    policy.links.foldLeft(""){(acc, e) => acc + e.destination.id + "_" + e.destination_input.name + " = " + e.source.id + "_" + e.source_output.name + ";\n"} + "}"
  }

  //TODO: Passer en private def
  //TODO: refactoring (code duplication with generatePeriodicSensor)
  def generateEventSensor(s:EventSensor[_<:DataType]) = {
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

  def generatePeriodicSensor(s:PeriodicSensor[_<:DataType]) = {
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


  def generateDataTypeName[T<:DataType](d:Class[T]) = {
    d.getSimpleName match {
      case "IntegerType" => "int"
      case "LongType" => "long"
      case "StringType" => "String"
      case "DoubleType" => "double"
      case "SmartCampusType" => "struct SmartCampus"
      case "SantanderParkingType" => "struct SantanderParkingType"
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
}
