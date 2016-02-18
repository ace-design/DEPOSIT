package fr.unice.modalis.cosmic.simulator

import com.typesafe.scalalogging.LazyLogging
import fr.unice.modalis.cosmic.deposit.core._

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.generator.{NodeDegreeRange, RandomGraph}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 17/02/2016.
  */




object RandomPolicy extends LazyLogging{

  case class RandomGraphProperties(order:Int, nodeDegrees: NodeDegreeRange) extends RandomGraph.IntFactory{
    override def connected = false
  }

  def convertGraphToDeposit(graph: Graph[Int, DiEdge]) = {
    val name = "simulatedPolicy_" + scala.util.Random.alphanumeric.take(5).mkString
    logger.debug("Generation of " + name + " has just started")
    var policy:Policy = new Policy(name, Set(), Set(), Set())

    val concepts = graph.nodes.map{n => SimulatedConceptFactory(n.inDegree, n.outDegree, classOf[SmartCampusType])}
    concepts.foreach(c => policy = policy.add(c))
    logger.debug(concepts.size + " concepts has been generated")
    val outputs = policy.getNonConnectedOutputPorts.asInstanceOf[Set[Output[SmartCampusType]]]
    val inputs = policy.getNonConnectedInputPorts.asInstanceOf[Set[Input[SmartCampusType]]]

    val allPossibleflows = (for (output <- outputs; input <- inputs) yield (output, input)).map{e => Flow(e._1.asInstanceOf[Output[SmartCampusType]], e._2.asInstanceOf[Input[SmartCampusType]])}.filterNot(f => f.source == f.destination)
    logger.debug(allPossibleflows.size + " data flows are possible")

    var previousResult = (0,0)
    while(policy.getNonConnectedInputPorts.nonEmpty) {
      val remaining = policy.getNonConnectedInputPorts.size
      logger.info("Remaining input ports to connect: " + remaining)


      if (previousResult._2 < 5) {
        //Select randomly a flow
        val flow = allPossibleflows.toVector(scala.util.Random.nextInt(allPossibleflows.size))

        // Check if destination port is not yet connected
        val free = !policy.flows.exists { f => f.destination_input == flow.destination_input }
        if (policy.add(flow).toGraph.isAcyclic && free)
          policy = policy.add(flow)

        // Updating the invariants
        if (previousResult._1 == remaining)
          previousResult = (previousResult._1, previousResult._2 + 1)
        else
          previousResult = (remaining, 0)


      } else { // We are probably in a blocked situation
        // Add a sensor in the policy to lighten the generation process
        policy = policy.add(SimulatedConceptFactory.apply(0, 1, classOf[SmartCampusType]))
        previousResult = (remaining, 0)
      }

    }

    // Final optimizations

    // Delete isolated nodes
    val isolated = policy.concepts -- (policy.flows.map(_.source) ++ policy.flows.map(_.destination))
    logger.warn(s"Isolated concepts: $isolated")
    isolated.foreach(c => policy = policy.delete(c))

    // Connect leaf operation to collectors
    val leafOperations = policy.concepts.filter(c => !c.isInstanceOf[Collector[_]] && (policy.flowsFrom(c) equals Set.empty))
    logger.warn(s"Leaf operations: $leafOperations")

    leafOperations.foreach {operation => {
      val newCollector = SimulatedConceptFactory(1, 0, classOf[SmartCampusType]).asInstanceOf[SimulatedCollector[_ <:DataType]]
      policy = policy.add(newCollector).add(new Flow(operation.asInstanceOf[SimulatedOperation[_ <:DataType,_ <:DataType]].getOutput("o1"), newCollector.input))
    }}
    policy
  }

  def apply(nodes:Int = 10, maxDegree:Int = 5) = {
    logger.debug(s"Generating a random policy with $nodes nodes and $maxDegree as max degree")
    assert(nodes > 1 && maxDegree > 1)
    convertGraphToDeposit(RandomGraph.diGraph(Graph, RandomGraphProperties(nodes, NodeDegreeRange(1,maxDegree))).draw)
  }
}


