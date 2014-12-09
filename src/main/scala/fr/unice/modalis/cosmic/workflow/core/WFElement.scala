package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException

/**
 * Represent a WorkFlow Element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFElement {
  // Component input list
  val inputs:List[Input[_<:DataType]]

  // Component output list
  val outputs:List[Output[_<:DataType]]

  // Unique identifier
  val id:String = "WFElement_" + scala.util.Random.alphanumeric.take(5).mkString

  // Merge operator
  def +(e : WFElement):WFElement // Naive implementation = Set(this, e)

  
  def uid:String = toString + "_" + id

}
