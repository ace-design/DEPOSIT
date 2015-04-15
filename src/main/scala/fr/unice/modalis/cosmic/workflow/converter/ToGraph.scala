package fr.unice.modalis.cosmic.workflow.converter

import fr.unice.modalis.cosmic.workflow.core.{Workflow, Element}
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
/**
 * Translate a workflow into a directed graph with Graph for Scala
 * Created by Cyril Cecchinel - I3S Laboratory on 03/03/15.
 */
object ToGraph {

  def apply(w:Workflow) = generateGraph(w)

  def generateGraph(w:Workflow) = {
    val nodes = w.ios ++ w.activities
    val edges = w.links.map(l => l.source ~> l.destination)

    Graph.from(nodes, edges)

  }

}
