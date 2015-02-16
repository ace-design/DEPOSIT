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
}

/**
 * Workflow component input
 * @tparam T Data type
 */
case class Input[T <: DataType](var name:String) extends Port[T] {
  def this() = this("in_" + Random.alphanumeric.take(5).mkString)


  val way = IO.IN
  def setName(n: String) = name = n // /!\ Mutable field, PoC Only
}

/**
 * Workflow component output
 * @tparam T Data type
 */
case class Output[T <: DataType](var name:String) extends Port[T] {
  def this() = this("out_" + Random.alphanumeric.take(5).mkString)

  val way = IO.OUT
  def setName(n: String) = name = n // /!\ Mutable field, PoC Only

}
