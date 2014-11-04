package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{WFLink, Workflow, WFElement, DataType}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */

/**
 * Represent the action of deleting an element
 * @param e Element to add
 * @tparam T Data type of the element
 */
case class DeleteElement[T<:DataType](e: WFElement[T]) extends Instruction[T]{

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow[T]): Workflow[T] = w.deleteElement(e)

  override def toString:String = "{DeleteElement element=" + e + "}"
}

/**
 * Represent the action of deleting a link
 * @param l Link to add
 * @tparam T Data type of the link
 */
case class DeleteLink[T<:DataType](l:WFLink[T]) extends Instruction[T]{
  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  override def make(w: Workflow[T]): Workflow[T] = w.deleteLink(l)

  override def toString:String = "{DeleteLink link=" + l + "}"
}