package fr.unice.modalis.cosmic.workflow.core

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataIO[T<:DataType] extends WFElement{
  override val id:String = "io" + scala.util.Random.alphanumeric.take(5).mkString


}

trait WorkflowStub[T<:DataType] extends DataIO[T]

case class WorkflowStubInput[T<:DataType]() extends WorkflowStub[T]{
  val output = new Output[T](this)
  override def toString:String = "WFSTUB_INPUT"
}

case class WorkflowStubOutput[T<:DataType]() extends WorkflowStub[T]{
  val input = new Input[T](this)
  override def toString:String = "WFSTUB_OUTPUT"
}

/**
 * Workflow source
 * @tparam T Data type
 */
trait Sensor[T<:DataType] extends DataIO[T]{
  val url:String
  val output = new Output[T](url, this)


  override def toString:String = "SENSOR[" + url + "]"
}

/**
 * Periodic sensors
 * @param wishedPeriod Sensor period
 * @param url Sensor URL
 * @tparam T Data type
 */
case class PeriodicSensor[T<:DataType](val wishedPeriod:Int, override val url: String) extends Sensor[T] {
  override def toString:String = "PERIODIC_SENSOR[" + wishedPeriod + ";" + url + "]"

}

/**
 * Event based sensor
 * @param url Sensor URL
 * @tparam T Data type
 */
case class EventSensor[T<:DataType](override val url:String) extends Sensor[T] {
  override def toString:String = "EVENT_SENSOR[" + url + "]{" + id + "}"
}
/**
 * Workflow sink. Refers to a collector
 * @param endpoint Collector URL
 * @tparam T Data type
 */
case class Collector[T<:DataType](endpoint:String) extends DataIO[T] {
  val input = new Input[T](endpoint, this)

  override def toString:String = "COLLECTOR[" + endpoint + "]"
}
