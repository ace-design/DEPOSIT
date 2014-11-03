package fr.unice.modalis.cosmic.workflow.core

import scala.util.Random

/**
 * Workflow component trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Component extends WFElement {

}

/**
 * Workflow component input
 * @param name Input name
 * @param component Reference to component
 * @tparam T Data type
 */
class Input[T <: DataType](val name:String, val component:WFElement) {
  def this(component:WFElement) = this("input_" + Random.alphanumeric.take(5).mkString, component)
}

/**
 * Workflow component output
 * @param name Output name
 * @param component Reference to component
 * @tparam T Data type
 */
class Output[T <: DataType](val name:String, val component:WFElement) {
  def this(component:WFElement) = this("output_" + Random.alphanumeric.take(5).mkString, component)
}
