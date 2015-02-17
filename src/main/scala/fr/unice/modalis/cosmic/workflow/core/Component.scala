package fr.unice.modalis.cosmic.workflow.core


import scala.util.Random



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
trait Port[T <: DataType] {
  val way:IO.ComponentIO
  val parent:WFElement
}

/**
 * Workflow component input
 * @tparam T Data type
 */
case class Input[T <: DataType](var name:String, val parent:WFElement) extends Port[T] {

  def this(parent:WFElement) = this("input_" + scala.util.Random.alphanumeric.take(5).mkString, parent)
  val way = IO.IN
  def setName(n: String) = name = n // /!\ Mutable field, PoC Only
}

/**
 * Workflow component output
 * @tparam T Data type
 */
case class Output[T <: DataType](var name:String, val parent:WFElement) extends Port[T] {
  def this(parent:WFElement) = this("output_" + scala.util.Random.alphanumeric.take(5).mkString, parent)
  val way = IO.OUT
  def setName(n: String) = name = n // /!\ Mutable field, PoC Only

}
