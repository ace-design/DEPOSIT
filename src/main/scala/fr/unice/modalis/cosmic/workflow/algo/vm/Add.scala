package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{WFElement, WFLink, Workflow}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */

/**
 * Represent the action of adding an element
 * @param component Element to add
 */
case class AddElement(val component: WFElement) extends Instruction{

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow): Workflow = w.addElement(component)

  override def toString:String = "{AddElement element=" + component + "}"
}

/**
 * Represent the action of adding a link
 * @param component Link to add
 */
case class AddLink(val component:WFLink) extends Instruction{
  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow): Workflow = w.addLink(component)

  override def toString:String = "{AddLink link=" + component + "}"
}

