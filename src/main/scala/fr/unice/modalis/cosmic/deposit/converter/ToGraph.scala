package fr.unice.modalis.cosmic.deposit.converter

import fr.unice.modalis.cosmic.deposit.core.{Concept, Policy}

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
/**
 * Translate a workflow into a directed graph with Graph for Scala
 * Created by Cyril Cecchinel - I3S Laboratory on 03/03/15.
 */
object ToGraph {

  def apply(w:Policy) = generateGraph(w)

  def generateGraph(w:Policy) = {
    val nodes = w.ios ++ w.operations
    val edges = w.flows.map(l => l.source ~> l.destination)

    Graph.from[Concept, DiEdge](nodes, edges)

  }

}
