package fr.unice.modalis.cosmic.deployment.infrastructure

import fr.unice.modalis.cosmic.deployment.network.{Edge, Entity, Sensor}
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition

/**
  * An infrastructure model is composed by
  *   * A network topology
  *   * A platform features description (contained in the network topology)
  *   * A deployment strategy
  *
  * Created by Cyril Cecchinel - I3S Laboratory on 07/01/2016.
  */


case class NetworkTopology(name:String, resources:Set[Entity], edges:Set[Edge]) {

  def getSensorsFromNode(n: String):Set[Sensor] = {
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

}



case class InfrastructureModel(topology:NetworkTopology, strategy:DeploymentRepartition)
