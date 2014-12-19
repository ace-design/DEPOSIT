package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{WFElement, WFLink, Workflow}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */

/**
 * Represent the action of deleting an element
 * @param component Element to add
 */
case class DeleteElement(val component: WFElement) extends Instruction {

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow): Workflow = w.deleteElement(component)

  override def toString: String = "{DeleteElement element=" + component + "}"
}

/**
 * Represent the action of deleting a link
 * @param component Link to add
 */
case class DeleteLink(val component: WFLink) extends Instruction {
  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow): Workflow = w.deleteLink(component)

  override def toString: String = "{DeleteLink link=" + component + "}"
}