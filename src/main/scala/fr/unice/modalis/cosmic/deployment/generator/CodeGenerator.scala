package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core.{PeriodicSensor, Policy}
import org.chocosolver.solver.Solver
import org.chocosolver.solver.constraints.IntConstraintFactory
import org.chocosolver.solver.variables.VariableFactory

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

  def apply(p:Policy) = generate(p)

  def generateDataStructures(p:Policy):String

  def generatePolicyBody(policy: Policy):String

  def generateGlobalVariables(policy: Policy):String

  def generateInputs(policy: Policy):(String,String)

  def generate(p:Policy) = {
    var generatedCode = Source.fromFile(templateFile).getLines().mkString("\n")
    generatedCode = replace("data_structures", generateDataStructures(p), generatedCode)
    generatedCode = replace("datacollectionpolicy", generatePolicyBody(p), generatedCode)
    generatedCode = replace("global_variables", generateGlobalVariables(p), generatedCode)
    val inputsTxt = generateInputs(p)
    generatedCode = replace("sensor_instructions", inputsTxt._1, generatedCode)
    generatedCode = replace("dataacquisition", inputsTxt._2, generatedCode)
    generatedCode
  }
  
  def replace(parameter:String, value:String, source:String):String = source.replace("#@" + parameter + "@#", value)

  
  
  

  def orderedGenerationList(p:Policy) = {
    val totalConcepts = p.ios.size + p.operations.size
    val solver = new Solver("Generation ordering problem")
    val sourcesvariablesChoco = for (s <- p.sources) yield { VariableFactory.fixed(s.id, 1, solver)}
    val operationvariablesChoco = for (o <- p.operations) yield { VariableFactory.bounded(o.id, 2, totalConcepts, solver)}
    val collectorvariablesChoco = for (c <- p.collectors) yield {VariableFactory.fixed(c.id, totalConcepts, solver)}

    val variablesChoco = sourcesvariablesChoco ++ operationvariablesChoco ++ collectorvariablesChoco

    for (l <- p.links) yield {solver.post(IntConstraintFactory.arithm(variablesChoco.find(_.getName equals l.source.id).get, "<", variablesChoco.find(_.getName equals l.destination.id).get))}

    if (solver.findSolution()) {
      val namedOperationsOrder = solver.retrieveIntVars().map(v => (v.getValue, v.getName)).toList.sortBy(_._1).map(_._2).map(p.findConceptById(_).get)

      p.sources.toList ++ namedOperationsOrder
    }
    else {
      throw new NonGenerableException(p)
    }

  }

  def computePeriod(p:Policy) = {
    //Compute the data acquisition period (lcm of overall periodic sensor periods)
    Utils.lcmm(p.sources.collect({case x:PeriodicSensor[_] => x}).map{_.wishedPeriod}.toList)
  }
}

case class NonHandledSensorException(io:Any) extends Exception("Non handled sensor " + io)

case class NonGenerableException(p:Policy) extends Exception(p.name + " is not generable")

case class Variable(name:String, t:String)
case class Instruction(inputs:Set[Variable], body:String, outputs:Set[Variable])