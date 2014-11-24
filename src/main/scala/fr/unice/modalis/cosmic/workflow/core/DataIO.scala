package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataIO[T<:DataType] extends WFElement[T] {


}

/**
 * Workflow source
 * @param sensor Sensor ID
 * @tparam T Data type
 */
case class Source[T<:DataType](sensor:String) extends DataIO[T]{
  val output = new Output[T](this)

  override val inputs: List[Input[T]] = List()
  override val outputs: List[Output[T]] = List(output)

}

/**
 * Workflow sink. Refers to a collector
 * @param url Collector URL
 * @tparam T Data type
 */
case class Sink[T<:DataType](url:String) extends DataIO[T] {
  val input = new Input[T](this)

  override val inputs: List[Input[T]] = List(input)
  override val outputs: List[Output[T]] = List()






}
