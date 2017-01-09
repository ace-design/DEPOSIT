package fr.unice.modalis.cosmic.deployment.generator

import java.io.PrintWriter

import fr.unice.modalis.cosmic.deployment.generator.ProcessingGenerator._
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.ProgrammingLanguage.ProgrammingLanguage
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deposit.core._
import org.chocosolver.solver.Solver
import org.chocosolver.solver.constraints.IntConstraintFactory
import org.chocosolver.solver.variables.VariableFactory

import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Code generator trait
 * Contains common methods shared by all code generators
 * Created by Cyril Cecchinel - I3S Laboratory on 05/10/15.
 */
trait CodeGenerator {
  // Path to template file
  val templateFile: String

  val CURRENT_TIMESTAMP_METHOD:String

  val language:ProgrammingLanguage

  val sensorTypeHandling:HashMap[SensorType.Value, (String, String, String)]
  val sensorBrandHandling:HashMap[SensorBrand.Value, (String, String)]

  def apply(p:Policy) = generate(p)


  def generateIntraMessage(dataType: DataType):String

  def generateDataStructure(dataType: DataType):String

  /**
   * Generate associated Data Structures
   * @param p Data collection policy
   * @return Compilable code defining data structures
   */
  def generateDataStructures(p:Policy):String

  /**
   * Generate the executable data collection policy
   * @param policy Data collection policy
   * @return Compilable code defining the policy body
   */
  def generatePolicyBody(policy: Policy):String

  /**
   * Generate global variables
   * @param policy Data collection policy
   * @return Compilable code defining global variables
   */
  def generateGlobalVariables(policy: Policy):String

  /**
   * Generate the data inputs
   * @param policy Data collection policy
   * @return Compilable code defining data inputs
   */
  def generateInputs(policy: Policy):(String,String)

  /**
   * Generate constant
   * @param s Sensor data value
   * @return Compilable code defining a constant
   */
  def generateConstant(s:DataType):String

  /**
   * Building an instruction from a concept
   * @param c Concept
   * @param policy Data collection policy
   * @tparam T Input sensor data type
   * @tparam U Output sensor data type
   * @return An instruction
   */
  def generateInstruction[T<:SensorDataType, U<:SensorDataType](c:Concept, policy: Policy):Instruction


  /**
   * Produce a compilable source file
   * @param name File name
   * @param code Compilable code
   */
  def produceSourceFile(name:String, code:String):Unit

  /**
   * Generate data collection policy
   * @param p Data collection policy
   * @return Compilable code defining the data collection policy
   */
  def generate(p:Policy) = {
    var generatedCode = Source.fromFile(templateFile).getLines().mkString("\n")
    generatedCode = replace("data_structures", generateDataStructures(p), generatedCode)
    generatedCode = replace("global_variables", generateGlobalVariables(p), generatedCode)
    //generatedCode = replace("null_values", generateNullValues(p), generatedCode)
    generatedCode = replace("datacollectionpolicy", generatePolicyBody(p), generatedCode)
    val inputsTxt = generateInputs(p)
    generatedCode = replace("sensor_instructions", inputsTxt._1, generatedCode)
    generatedCode = replace("dataacquisition", inputsTxt._2, generatedCode)
    generatedCode = replace("flush", generateFlushMethod(p), generatedCode)
    if (p.hasProperty("board").isDefined)
      generatedCode = replace("board_id", "\"" + p.readProperty("board").get.asInstanceOf[String] + "\"", generatedCode)
    generatedCode
  }

  /**
   * Replace method
   * @param parameter Parameter
   * @param value Value
   * @param source Source
   * @return Source code with parameter replaced by its value
   */
  def replace(parameter:String, value:String, source:String):String = source.replace("#@" + parameter + "@#", value)

  def apply(p:Policy, toFile:Boolean = false) = {
    if (toFile) produceSourceFile(p.name, generate(p)) else generate(p)
  }

  /**
   * Compute an ordered generation list (with Choco solver)
   * @param p Data collection policy
   * @return An ordered generation list
   */
  def orderedGenerationList(p:Policy) = {
    val totalConcepts = p.ios.size + p.operations.size
    val solver = new Solver("Generation ordering problem")
    val inputVariablesChoco = for (s <- p.inputs) yield { VariableFactory.fixed(s.id, 1, solver)}
    val operationVariablesChoco = for (o <- p.operations) yield { VariableFactory.bounded(o.id, 2, totalConcepts, solver)}
    val outputVariablesChoco = for (c <- p.outputs) yield {VariableFactory.fixed(c.id, totalConcepts, solver)}

    val variablesChoco = inputVariablesChoco ++ operationVariablesChoco ++ outputVariablesChoco

    for (l <- p.flows) yield {solver.post(IntConstraintFactory.arithm(variablesChoco.find(_.getName equals l.source.id).get, "<", variablesChoco.find(_.getName equals l.destination.id).get))}

    if (solver.findSolution()) {
      val namedOperationsOrder = solver.retrieveIntVars().map(v => (v.getValue, v.getName)).toList.sortBy(_._1).map(_._2).map(p.findConceptById(_).get)

      p.sources.toList ++ namedOperationsOrder
    }
    else {
      throw new NonGenerableException(p)
    }

  }

  /**
    * Return if the policy has periodic sensors
    * @return True if the policy has periodic sensors, false otherwise
    */
  def hasPeriodicSensors(policy: Policy) = policy.sources.collect{case x:PeriodicSensor[_] => x}.nonEmpty

  /**
   * Compute the period of a policy (lcm of periodic sensors)
   * @param p Data collection policy
   * @return Data collection policy's period
   */
  def computePeriod(p:Policy) = {
    //Compute the data acquisition period (lcm of overall periodic sensor periods)
    Utils.lcmm(p.sources.collect({case x:PeriodicSensor[_] => x}).map{_.wishedPeriod}.toList)
  }
}

object CodeGenerator {
  def produceSourceFile(name:String, target:String, extension:String, code:String) = {
    val file = new PrintWriter("out/"+ target + "/" + name + "." + extension.replace(".", ""))
    file.println(code)
    file.close()

  }
}

case class IntraMessage(t:Long, src:String, data: DataType)

case class NonHandledSensorException(io:Any) extends Exception("Non handled sensor " + io)

case class NonGenerableException(p:Policy) extends Exception(p.name + " is not generable")

/**
 * Define a variable
 * @param name Name
 * @param t Data type
 */
case class Variable(name:String, t:String)

/**
 * Define an instruction
 * @param inputs Set of input variables
 * @param body Instruction body
 * @param outputs Set of output variables
 */
case class Instruction(inputs:Set[Variable], body:String, outputs:Set[Variable])

