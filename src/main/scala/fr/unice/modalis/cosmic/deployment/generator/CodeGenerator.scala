package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core.Policy
import org.chocosolver.solver.Solver
import org.chocosolver.solver.constraints.IntConstraintFactory
import org.chocosolver.solver.variables.VariableFactory

/**
 * Code generator trait
 * Contains common methods shared by all code generators
 * Created by Cyril Cecchinel - I3S Laboratory on 05/10/15.
 */
trait CodeGenerator {
  def replace(parameter:String, value:String, source:String):String = source.replace("#@" + parameter + "@#", value)

  case class Variable(name:String, t:String)
  case class Instruction(inputs:Set[Variable], body:String, outputs:Set[Variable])

  def orderedGenerationList(p:Policy) = {
    val totalConcepts = p.ios.size + p.operations.size
    val solver = new Solver("Generation ordering problem")
    val sourcesvariablesChoco = for (s <- p.sources) yield { VariableFactory.fixed(s.id, 1, solver)}
    val operationvariablesChoco = for (o <- p.operations) yield { VariableFactory.bounded(o.id, 2, totalConcepts, solver)}
    val collectorvariablesChoco = for (c <- p.collectors) yield {VariableFactory.fixed(c.id, totalConcepts, solver)}

    val variablesChoco = sourcesvariablesChoco ++ operationvariablesChoco ++ collectorvariablesChoco

    for (l <- p.links) yield {solver.post(IntConstraintFactory.arithm(variablesChoco.find(_.getName equals l.source.id).get, "<", variablesChoco.find(_.getName equals l.destination.id).get))}
    solver.post(IntConstraintFactory.alldifferent(operationvariablesChoco.toArray))
    
    if (solver.findSolution()) {
      val namedOperationsOrder = solver.retrieveIntVars().map(v => (v.getValue, v.getName)).toList.sortBy(_._1).map(_._2).map(p.findConceptById(_).get)
      p.sources.toList ++ namedOperationsOrder ++ p.collectors.toList
    }
    else {
      throw new NonGenerableException(p)
    }

  }
}

case class NonHandledSensorException(io:Any) extends Exception("Non handled sensor " + io)

case class NonGenerableException(p:Policy) extends Exception(p.name + " is not generable")