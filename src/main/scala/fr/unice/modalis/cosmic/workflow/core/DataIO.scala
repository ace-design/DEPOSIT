package fr.unice.modalis.cosmic.workflow.core

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataIO extends WFElement {

}

/**
 * Workflow source
 * @param sensor Sensor ID
 * @tparam T Data type
 */
case class Source[T<: DataType](val sensor:String) extends DataIO{
  val output = new Output[T](this)
}

/**
 * Workflow sink. Refers to a collector
 * @param url Collector URL
 * @tparam T Data type
 */
case class Sink[T<: DataType](val url:String) extends DataIO{
  val input = new Input[T](this)
}
