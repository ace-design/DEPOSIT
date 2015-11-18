package fr.unice.modalis.cosmic.deployment.heuristics

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{GenericNode, NetworkTopology, Sensor}
import fr.unice.modalis.cosmic.deposit.core.Concept


/**
  * We specify the different heuristics for the deployment over a sensing infrastructure
  * Created by Cyril Cecchinel - I3S Laboratory on 17/11/2015.
  */
trait DeploymentHeuristic {
  /**
    * Place a concept over a network topology
    * @param concept Concept to place
    * @param networkTopology Network topology
    * @return Where the concept should be placed
    */
  def place(concept: Concept, networkTopology: NetworkTopology):GenericNode
}


object DeploymentHeuristic {
  val CLOSER_TO_THE_SENSORS = CloserToTheSensorsHeuristic
}

/**
  * This heuristic place the concepts with a "closer to the sensors" property
  */
object CloserToTheSensorsHeuristic extends DeploymentHeuristic {

  override def place(concept: Concept, networkTopology: NetworkTopology): GenericNode = {
    // Generate the oriented weighted topology graph
    val graph = networkTopology.toGraph

    def n(outer:GenericNode): graph.NodeT = graph get outer

    // List of sensors
    val sensors = networkTopology.resources.collect {case x:Sensor => x}
    // List of others resources
    val others = networkTopology.resources -- sensors

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
    averageDistanceFromSensors.filterKeys(k => possibleTargetsForConcept contains k).minBy(_._2)._1

  }
}
