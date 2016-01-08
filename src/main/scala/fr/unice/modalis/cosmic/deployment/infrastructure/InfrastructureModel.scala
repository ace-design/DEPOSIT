package fr.unice.modalis.cosmic.deployment.infrastructure

import fr.unice.modalis.cosmic.deployment.heuristics.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.network.dsl.kernel._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

/**
  * An infrastructure model is composed by
  *   * A network topology
  *   * A platform features description (contained in the network topology)
  *   * A deployment strategy
  *
  * Created by Cyril Cecchinel - I3S Laboratory on 07/01/2016.
  */


case class NetworkTopology(resources:Set[Entity], edges:Set[Edge]) {

  def getSensorsFromNode(n: Entity):Set[Sensor] = {
    var visited = List[Entity]()
    def inner(n: Entity):List[Sensor] = {
      visited = n :: visited

      n.sensors.toList ::: (isConnectedBy(n).toList match {
        case Nil => List()
        case x :: xl => x.sensors.toList ::: xl.foldLeft(List[Sensor]()) {
          (acc, n) => if (!visited.contains(n)) {
            visited = n :: visited
            inner(n) ::: acc
          } else acc
        }
      })
    }

    inner(n).toSet
  }

  def isConnectedTo(n: Entity) = edges.filter(_.source == n).map(_.destination)
  def isConnectedBy(n: Entity) = edges.filter(_.destination == n).map(_.source)

  def toGraph = {
    val _nodes = resources
    val _edges = edges.map(l => l.source ~> l.destination % 1) //At the moment, we assume all edges weighted with the same weight
    Graph.from(_nodes, _edges)
  }
}



case class InfrastructureModel(topology:NetworkTopology, strategy:DeploymentRepartition)
