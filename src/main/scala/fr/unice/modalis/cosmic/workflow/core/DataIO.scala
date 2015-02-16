package fr.unice.modalis.cosmic.workflow.core

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataIO[T<:DataType] extends WFElement{


}

/**
 * Workflow source
 * @param url Sensor ID
 * @tparam T Data type
 */
case class Sensor[T<:DataType](url:String) extends DataIO[T]{
  val output = new Output[T](url, this)


  override def toString:String = "SENSOR[" + url + "]{" + id + "}"
}

class PeriodicSensor[T<:DataType](val period:Int, override val url: String) extends Sensor[T](url)

class EventSensor[T<:DataType](override val url:String) extends Sensor[T](url)
/**
 * Workflow sink. Refers to a collector
 * @param endpoint Collector URL
 * @tparam T Data type
 */
case class Collector[T<:DataType](endpoint:String) extends DataIO[T] {
  val input = new Input[T](endpoint, this)

  override def toString:String = "COLLECTOR[" + endpoint + "]{" + id + "}"
}
