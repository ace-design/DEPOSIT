package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deployment.infrastructure.Features.CommunicationWay
import fr.unice.modalis.cosmic.deployment.network.Entity

/**
 * Input/Output trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */

/**
 * Policy I/O
 * @tparam T Datatype
 */
trait PolicyIO[T<:DataType] extends Concept{
  override val id:String = "io" + scala.util.Random.alphanumeric.take(5).mkString
  val name:String
  val dataType:Class[T]
}

/**
 * Data I/O
 * @tparam T DataType
 */
trait DataIO[T<:DataType] extends PolicyIO[T]{

}

trait DataInput[T<:DataType] extends DataIO[T]{
  val output:Output[T]
  override def placingConstraintsEquation(e:Entity) = e.communication.exists(_.cWay == CommunicationWay.In)
}


trait DataOutput[T<:DataType] extends DataIO[T] {
  val input:Input[T]
  override def placingConstraintsEquation(e:Entity) = e.communication.exists(_.cWay == CommunicationWay.Out)

}



case class GenericInput[T<:DataType](name:String, dataType: Class[T]) extends DataInput[T] {
  val output = new Output[T](name, this, dataType)
  override val commonName: String = name

  /**
   * Return a copy of this concept (with different id)
    *
    * @return copy of this concept
   */
  override def duplicate: GenericInput[T] = new GenericInput[T](name, dataType)

  val url: String = ""

  override def ~=(x: Any): Boolean = x.isInstanceOf[GenericInput[T]] &&
    (x.asInstanceOf[GenericInput[T]].name equals this.name) &&
    (x.asInstanceOf[GenericInput[T]].dataType equals this.dataType)
}

case class GenericOutput[T<:DataType](name:String, dataType: Class[T]) extends DataOutput[T] {
  val input = new Input[T](name, this, dataType)
  val url: String = name
  override val commonName: String = name

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: GenericOutput[T] = new GenericOutput[T](name, dataType)

  override def ~=(x: Any): Boolean = x.isInstanceOf[GenericOutput[T]] &&
    (x.asInstanceOf[GenericOutput[T]].name equals this.name) &&
    (x.asInstanceOf[GenericOutput[T]].dataType equals this.dataType)
}



trait JoinPoint[T<:DataType] extends PolicyIO[T]

case class JoinPointInput[I<:DataType](toConceptInput:Input[I], dataType: Class[I]) extends JoinPoint[I] with DataInput[I]{
  val output = new Output[I](this, dataType)
  override def toString:String = "JOIN_POINT_INPUT[to=" + toConceptInput + "]"

  override val name: String = "JoinPoint" + id
  override val commonName: String = "JOIN_POINT_INPUT"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new JoinPointInput[I](toConceptInput, dataType)

  override def placingConstraintsEquation(e:Entity) = e.communication.exists(_.cWay == CommunicationWay.In)

  override def ~=(x: Any): Boolean = x.isInstanceOf[JoinPointInput[I]] &&
    (x.asInstanceOf[JoinPointInput[I]].toConceptInput equals this.toConceptInput) &&
    (x.asInstanceOf[JoinPointInput[I]].dataType equals this.dataType)

}

case class JoinPointOutput[O<:DataType](fromConceptOutput:Output[O], dataType: Class[O]) extends JoinPoint[O] with DataOutput[O]{
  val input = new Input[O](this, dataType)
  override def toString:String = "JOIN_POINT_OUTPUT[from=" + fromConceptOutput + "]"
  override val name: String = "JointPoint" + id
  override val commonName: String = "JOIN_POINT_OUTPUT"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: JoinPointOutput[O] = new JoinPointOutput[O](fromConceptOutput, dataType)

  override def placingConstraintsEquation(e:Entity) = e.communication.exists(_.cWay == CommunicationWay.Out)

  override def ~=(x: Any): Boolean = x.isInstanceOf[JoinPointOutput[O]] &&
    (x.asInstanceOf[JoinPointOutput[O]].fromConceptOutput equals this.fromConceptOutput) &&
    (x.asInstanceOf[JoinPointOutput[O]].dataType equals this.dataType)
}


/**
 * Workflow source
 * @tparam T Data type
 */
trait Sensor[T<:DataType] extends DataInput[T]{
  val url:String
  val output = new Output[T](url, this, dataType)
  override val name = url
  val dataType: Class[T]
  override def placingConstraintsEquation(e:Entity) = e.sensors.map(_.name) contains url

  /**
    * Check if two sensors are similar
    * @return True if similar, false otherwise
    */

  override def toString:String = "SENSOR[" + url + "]"
}

/**
 * Periodic sensors
 * @param wishedPeriod Sensor period
 * @param url Sensor URL
 * @tparam T Data type
 */
case class PeriodicSensor[T<:DataType](wishedPeriod:Int, override val url: String, dataType: Class[T]) extends Sensor[T] {

  override val commonName: String = "PERIODIC_SENSOR[" + wishedPeriod + ";" + url + "]"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: PeriodicSensor[T] = new PeriodicSensor[T](wishedPeriod, url, dataType)

  /**
    * Check if two sensors are similar
    * @return True if similar, false otherwise
    */
  override def ~=(x: Any): Boolean = x.isInstanceOf[PeriodicSensor[T]] &&
    (x.asInstanceOf[PeriodicSensor[T]].dataType equals this.dataType) &&
    (x.asInstanceOf[PeriodicSensor[T]].url equals this.url) &&
    (x.asInstanceOf[PeriodicSensor[T]].wishedPeriod equals this.wishedPeriod)

}

/**
 * Event based sensor
 * @param url Sensor URL
 * @tparam T Data type
 */
case class EventSensor[T<:DataType](override val url:String, dataType: Class[T]) extends Sensor[T] {

  override val commonName: String = "EVENT_SENSOR[" + url + "]"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: EventSensor[T] = new EventSensor[T](url, dataType)

  /**
    * Check if two sensors are similar
    * @return True if similar, false otherwise
    */
  override def ~=(x: Any): Boolean = x.isInstanceOf[EventSensor[T]] &&
    (x.asInstanceOf[EventSensor[T]].dataType equals this.dataType) &&
    (x.asInstanceOf[EventSensor[T]].url equals this.url)
  }


/**
 * Workflow sink. Refers to a collector
 * @param endpoint Collector URL
 * @tparam T Data type
 */
case class Collector[T<:DataType](endpoint:String, dataType: Class[T]) extends DataOutput[T] {
  val input = new Input[T](endpoint, this, dataType)
  val name = endpoint

  override def toString:String = "COLLECTOR[" + endpoint + "]"
  override def placingConstraintsEquation(e:Entity) = true // e.communication contains Communication(CommunicationType.WAN, CommunicationWay.Out)
  override val commonName: String = toString


  /**
   * Return a copy of this concept (with different id)
    *
    * @return copy of this concept
   */
  override def duplicate: Collector[T] = new Collector[T](endpoint, dataType)

  override def ~=(x: Any): Boolean = x.isInstanceOf[Collector[T]] &&
    (x.asInstanceOf[Collector[T]].dataType equals this.dataType) &&
    (x.asInstanceOf[Collector[T]].endpoint equals this.endpoint)
}
