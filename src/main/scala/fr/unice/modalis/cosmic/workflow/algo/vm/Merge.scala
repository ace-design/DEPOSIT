package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{WFElement, WFLink, Workflow}

/**
 * Merge two elements in a given Workflow
 * Pre-requisite : two similar elements already present in the workflow
 * Created by Cyril Cecchinel - I3S Laboratory on 19/12/14.
 */
case class Merge(e1: WFElement, e2: WFElement) extends Instruction {
  require(e1 ~ e2)

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow): Workflow = {
    require(w.elements.contains(e1) && w.elements.contains(e2))
    var tempWorkflow = w
    // Merge e2 into e1 (e1 <-- e2)

    // Identity concerned links
    val linksSource = w.links.filter(l => l.source == e2)
    val linksDestination = w.links.filter(l => l.destination == e2)

    // Delete e2 (and all links refering e2 by the same way)
    tempWorkflow = tempWorkflow.deleteElement(e2)

    // Compute new links
    val newLinksAsSource = linksSource.map(l => new WFLink(e1.outputs.head, l.destination_input))
    val newLinksAsDestination = linksDestination.map(l => new WFLink(l.source_output, e1.inputs.head))

    newLinksAsSource.foreach(l => tempWorkflow = tempWorkflow.addLink(l))
    newLinksAsDestination.foreach(l => tempWorkflow = tempWorkflow.addLink(l))

    tempWorkflow
  }
}
