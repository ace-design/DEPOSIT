package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataIO[T<:DataType] extends WFElement{


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

  def ~(x: WFElement):Boolean = x.isInstanceOf[Source[T]] && x.asInstanceOf[Source[T]].sensor == sensor

  // Merge operator
  override def +(e: WFElement): WFElement = if (e ~ this) new Source[T](sensor) else throw new NonMergeableException

  override def toString:String = "Source_" + id
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

  def ~(x: WFElement):Boolean = x.isInstanceOf[Sink[T]] && x.asInstanceOf[Sink[T]].url == url

  // Merge operator
  override def +(e: WFElement): WFElement = if (e ~ this) new Sink[T](url) else throw new NonMergeableException

  override def toString:String = "Sink_" + id
}
