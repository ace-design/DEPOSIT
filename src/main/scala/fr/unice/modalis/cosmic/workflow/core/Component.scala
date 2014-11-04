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
  val name = way + "_" + Random.alphanumeric.take(5).mkString

}

/**
 * Workflow component input
 * @param parent Reference to component
 * @tparam T Data type
 */
class Input[T <: DataType](val parent:WFElement[T]) extends ComponentIO[T] {
  val way = IO.IN
}

/**
 * Workflow component output
 * @param parent Reference to component
 * @tparam T Data type
 */
class Output[T <: DataType](val parent:WFElement[T]) extends ComponentIO[T] {
  val way = IO.OUT

}


