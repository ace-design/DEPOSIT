package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core._

import scala.io.Source

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 02/10/15.
 */

object ArduinoGenerator extends CodeGenerator{


  val LAST_VALUE_PREFIX = "lastValue_"

  def generate(c:Concept):Instruction = c match {
    case a:DataInput[_] => Instruction(Set(), generateDataTypeName(a.dataType) + " " + a.output.name + " = " + LAST_VALUE_PREFIX + a.id + ";", Set(Variable(a.output.name, generateDataTypeName(a.dataType))))
    //TODO Arithmetic operation
    case a:Arithmetic[_] => Instruction(a.inputs.foldLeft(Set[Variable]()){(acc, e) => acc + Variable(a.id + "_" + e.name, generateDataTypeName(a.iType))}, "/* TODO Arithmetic operation */", Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))))
    case a:Conditional[_] => Instruction(Set(Variable(c.id + "_" + a.input.name, generateDataTypeName(a.iType))), "if (" + a.predicate + ") " + generateDataTypeName(a.oType) + " " + a.id + "_" + a.thenOutput.name + " = " + c.id + "_" + a.input.name + "; else " + generateDataTypeName(a.oType) + " " +  a.elseOutput.name + " = " + c.id + "_" + a.input.name + ";", Set(Variable(a.id + "_" + a.thenOutput.name, generateDataTypeName(a.oType))))
    case a:DataOutput[_] => Instruction(Set(Variable(a.id + "_" + a.input.name, generateDataTypeName(a.dataType))), "send(" + a.id + "_" + a.input.name + "," + a.name + ");", Set())
    case a:Extract[_, _] => Instruction(Set(Variable(a.id + "_" + a.input.name, generateDataTypeName(a.iType))), generateDataTypeName(a.oType) + " " + a.id + "_" + a.output.name + " = " + a.id + "_" + a.input.name + ".data." + a.field + ";", Set(Variable(a.id + "_" + a.output.name, generateDataTypeName(a.oType))))
    case _ => throw new Exception("Can't generate concept " + c + " for Arduino platform")
  }

  def generateInstructionList(p:Policy) = {
    orderedGenerationList(p).map {generate}
  }


  //TODO: Passer en private def
  //TODO: handling type
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
  //TODO: handling type

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


  def generateStruct(compositeType: CompositeType) = {
    "struct " + compositeType.name + "{\n" + compositeType.bindings.foldLeft(""){(acc, e) => acc + generateDataTypeName(e._2) + " " + e._1 + ";\n"} + "};"
  }

  def generateDataTypeName[T<:DataType](d:Class[T]) = {
    d.getSimpleName match {
      case "IntegerType" => "int"
      case "LongType" => "long"
      case "StringType" => "String"
      case "DoubleType" => "double"
      case "SmartCampusType" => "struct SmartCampus"
      case "SantanderParkingType" => "struct SantanderParkingType"
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
