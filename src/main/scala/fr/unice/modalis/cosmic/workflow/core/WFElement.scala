package fr.unice.modalis.cosmic.workflow.core

/**
 * Represent a WorkFlow Element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFElement {
  // Component input list
  val inputs: List[Input[_ <: DataType]]

  // Component output list
  val outputs: List[Output[_ <: DataType]]

  // Unique identifier
  var id: String = scala.util.Random.alphanumeric.take(5).mkString

  // Merge operator
  def +(e: WFElement): WFElement // Naive implementation = Set(this, e)

  // Similar operator
  def ~(x: WFElement): Boolean

  override def equals(x: Any) = x.isInstanceOf[WFElement] && x.asInstanceOf[WFElement].id == id

}
