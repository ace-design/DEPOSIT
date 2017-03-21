package fr.unice.modalis.cosmic.deployment.infrastructure

import fr.unice.modalis.cosmic.deployment.network.{Edge, Entity, Sensor}
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import org.chocosolver.solver.Solver
import org.chocosolver.solver.constraints.IntConstraintFactory
import org.chocosolver.solver.variables.VariableFactory

/**
  * An infrastructure model is composed by
  *   * A network topology
  *   * A platform features description (contained in the network topology)
  *   * A deployment strategy
  *
  * Created by Cyril Cecchinel - I3S Laboratory on 07/01/2016.
  */


case class NetworkTopology(name:String, resources:Set[Entity], edges:Set[Edge]) {

  def available(n: String):Set[Sensor] = {
    var visited = List[String]()
    def inner(n: String):List[Sensor] = {
      visited = n :: visited

      resources.find(_.name equals n).get.sensors.toList ::: isConnectedBy(n).toList.foldLeft(List[Sensor]()){
        (acc, n) => if (!visited.contains(n)) {
          visited = n :: visited
          inner(n) ::: acc
        } else acc
      }
    }

    inner(n).toSet
  }

  def isConnectedTo(n: String) = edges.filter(_.source == n).map(_.destination)
  def isConnectedBy(n: String) = edges.filter(_.destination == n).map(_.source)
  def findSensorByName(s:String) = resources.flatMap {_.sensors}.find(_.name equals s)
  def findEntityByName(s:String) = resources.find {_.name equals s}

  lazy val orderedTopology = {
    val solver = new Solver("Repartition problem")
    val totalEntities = resources.size

    val entitiesVariables = for (e <- resources) yield {VariableFactory.bounded(e.name, 1, totalEntities, solver)}
    for (l <- edges) yield solver.post(IntConstraintFactory.arithm(entitiesVariables.find(_.getName equals l.source).get, "<", entitiesVariables.find(_.getName equals l.destination).get))
    if (solver.findSolution())
      solver.retrieveIntVars().map(v => (v.getValue, v.getName)).toList.sortBy(_._1).map(_._2).map(findEntityByName(_).get)
     else
      throw new Exception("Unable to solve the repartition problem")
  }

  lazy val reachableSensors = resources.par.map{r => r.name -> available(r.name)}.seq.toMap
}



case class InfrastructureModel(topology:NetworkTopology, strategy:DeploymentRepartition)
