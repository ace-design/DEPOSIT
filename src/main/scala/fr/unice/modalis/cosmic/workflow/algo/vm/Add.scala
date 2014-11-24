package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{DataType, WFLink, WFElement, Workflow}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */

/**
 * Represent the action of adding an element
 * @param e Element to add
 * @tparam T Data type of the element
 */
case class AddElement[T<:DataType](e: WFElement[T]) extends Instruction[T]{

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow[T]): Workflow[T] = w.addElement(e)

  override def toString:String = "{AddElement element=" + e + "}"
}

/**
 * Represent the action of adding a link
 * @param l Link to add
 * @tparam T Data type of the link
 */
case class AddLink[T<:DataType](l:WFLink[T]) extends Instruction[T]{
  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow[T]): Workflow[T] = w.addLink(l)

  override def toString:String = "{AddLink link=" + l + "}"
}
