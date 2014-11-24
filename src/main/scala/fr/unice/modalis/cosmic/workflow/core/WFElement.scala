package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException

/**
 * Represent a WorkFlow Element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFElement[T<:DataType] {
  // Component input list
  val inputs:List[Input[T]]

  // Component output list
  val outputs:List[Output[T]]

  // Merge operator
  @throws(classOf[NonMergeableException])
  def +(e : WFElement[T]):Set[WFElement[T]] = Set(this, e)
}
