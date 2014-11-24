package fr.unice.modalis.cosmic.workflow.core


import scala.util.Random

/**
 * Workflow component trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Component[T<:DataType] extends WFElement[T] {

}

/**
 * Data way : IN/OUT
 */
object IO extends Enumeration {
  type ComponentIO = Value
  val IN, OUT = Value
}

/**
 * Component IO trait : represent a component IO interface
 * @tparam T Supported datatype
 */
trait ComponentIO[T <: DataType] {
  val way:IO.ComponentIO
  val parent:WFElement[T]

}

/**
 * Workflow component input
 * @param parent Reference to component
 * @tparam T Data type
 */
case class Input[T <: DataType](parent:WFElement[T], name:String) extends ComponentIO[T] {
  def this(parent:WFElement[T]) = this(parent,  "in_" + Random.alphanumeric.take(5).mkString)

  val way = IO.IN
}

/**
 * Workflow component output
 * @param parent Reference to component
 * @tparam T Data type
 */
case class Output[T <: DataType](parent:WFElement[T], name:String) extends ComponentIO[T] {
  def this(parent:WFElement[T]) = this(parent, "out_" + Random.alphanumeric.take(5).mkString)

  val way = IO.OUT

}


