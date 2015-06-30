package fr.unice.modalis.cosmic.deposit.core

/**
 * Data Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait PolicyIO[T<:DataType] extends Concept{
  override val id:String = "io" + scala.util.Random.alphanumeric.take(5).mkString
  val name:String
}

trait DataIO[T<:DataType] extends PolicyIO[T]{

}

trait DataInput[T<:DataType] extends DataIO[T]


trait DataOutput[T<:DataType] extends DataIO[T]



case class GenericInput[T<:DataType](val name:String) extends DataInput[T] {
  val output = new Output[T](name, this)
  val url: String = name
}

case class GenericOutput[T<:DataType](val name:String) extends DataOutput[T] {
  val input = new Input[T](name, this)
  val url: String = name
}



trait JoinPoint[T<:DataType] extends PolicyIO[T]

case class JoinPointInput[I<:DataType](val toConceptInput:Input[I]) extends JoinPoint[I]{
  val output = new Output[I](this)
  override def toString:String = "JOIN_POINT_INPUT[to=" + toConceptInput + "]"

  override val name: String = "JointPoint" + id
}

case class JoinPointOutput[O<:DataType](val fromConceptOutput:Output[O]) extends JoinPoint[O]{
  val input = new Input[O](this)
  override def toString:String = "JOIN_POINT_OUTPUT[from=" + fromConceptOutput + "]"
  override val name: String = "JointPoint" + id

}


/**
 * Workflow source
 * @tparam T Data type
 */
trait Sensor[T<:DataType] extends DataInput[T]{
  val url:String
  val output = new Output[T](url, this)
  override val name = url

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
 * Clock
 * @tparam T Data type
 */
case class Clock[T<:DataType]() extends DataInput[T] {
  override val name: String = "clock" + id
}

/**
 * Workflow sink. Refers to a collector
 * @param endpoint Collector URL
 * @tparam T Data type
 */
case class Collector[T<:DataType](endpoint:String) extends DataOutput[T] {
  val input = new Input[T](endpoint, this)
  val name = endpoint
  override def toString:String = "COLLECTOR[" + endpoint + "]"
}