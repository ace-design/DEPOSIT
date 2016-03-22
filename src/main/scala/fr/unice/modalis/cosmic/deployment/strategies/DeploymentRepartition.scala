package fr.unice.modalis.cosmic.deployment.strategies

import fr.unice.modalis.cosmic.deployment.infrastructure.NetworkTopology
import fr.unice.modalis.cosmic.deployment.network.{Entity, GenericNode}
import fr.unice.modalis.cosmic.deposit.core.Concept
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, RepositoryNotFoundException}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
/**
  * We specify the different heuristics for the deployment over a sensing infrastructure
  * Created by Cyril Cecchinel - I3S Laboratory on 17/11/2015.
  */
trait DeploymentRepartition {
  /**
    * Place a concept over a network topology
    * @param concept Concept to place
    * @param networkTopology Network topology
    * @return Where the concept should be placed
    */
  def place(concept: Concept, networkTopology: NetworkTopology):Entity
}


object DeploymentRepartition {
  val CLOSEST_TO_SENSORS = ClosestToSensorsRepartition
  val FREE_PLATFORMS = UseFreePlatforms
}

/**
  * UseFreePlatforms strategy
  * This strategy distribute in priority concepts on free platforms then apply Closest To the sensors strategy.
  * If no free platforms has been found, Closest to the sensors strategy is used
  */
object UseFreePlatforms extends DeploymentRepartition {

  override def place(concept: Concept, networkTopology: NetworkTopology): Entity = {
    // Proxy
    val proxy = RepositoriesManager.getRepository(networkTopology.name).getOrElse(throw new RepositoryNotFoundException(networkTopology.name))
    // A concept can be projected on
    val targets = concept.readProperty("targets").get.asInstanceOf[Set[Entity]]

    // Select free platforms
    val freeplatforms = for (t <- targets) yield {if (proxy.getPolicy(t).isEmpty) t}

    if (freeplatforms.nonEmpty){
      // If there are free platforms, replace targets with these platforms
      concept.addProperty("targets", freeplatforms.asInstanceOf[Set[Entity]])
    }

    ClosestToSensorsRepartition.place(concept, networkTopology)
  }
}
/**
  * This strategy places the concepts with a "closer to the sensors" property
  */
object ClosestToSensorsRepartition extends DeploymentRepartition {

  override def place(concept: Concept, networkTopology: NetworkTopology): Entity = {
    // Generate the oriented weighted topology graph
    val gNodes = networkTopology.resources.flatMap {_.sensors.map{_.name}} ++ networkTopology.resources.map{_.name}
    val gEdges = networkTopology.resources.flatMap(r => r.sensors.map {_.name}.map{_ ~> r.name % 1}) ++ networkTopology.edges.map{l => l.source.name ~> l.destination.name % 1}

    val graph = Graph.from(gNodes, gEdges)

    def n(outer:String): graph.NodeT = graph get outer

    // List of sensors
    val sensors = gNodes -- networkTopology.resources.map {_.name}
    // List of others resources
    val others = networkTopology.resources.map{_.name}

    val distanceFromSensors = (for (o <- others) yield {
      (o, for (s <- sensors) yield {
        n(s) shortestPathTo n(o) match {
          case None => (n(s), None)
          case Some(path) => (n(s), Some(path.edges.map(_.weight).sum))
        }
      })
    }).toMap


    val averageDistanceFromSensors = distanceFromSensors.map{ e => val values = e._2.toMap.values.flatten; (e._1, values.foldLeft(0.0)(_ + _) /values.foldLeft(0.0){(r,_) => r + 1})}


    val possibleTargetsForConcept = concept.readProperty("targets").get.asInstanceOf[Set[GenericNode]]

    // Find which concept is the closest to a sensor according the average distance
    val resultAsAString = averageDistanceFromSensors.filterKeys(k => possibleTargetsForConcept contains networkTopology.resources.find(_.name equals k).get).minBy(_._2)._1

    networkTopology.resources.find(_.name equals resultAsAString).get
  }
}
