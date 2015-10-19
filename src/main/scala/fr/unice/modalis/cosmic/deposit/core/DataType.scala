package fr.unice.modalis.cosmic.deposit.core

/**
 * Data Type trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataType {
  val id:String = "type" + scala.util.Random.alphanumeric.take(5).mkString
  val name:String


}
trait AtomicType extends DataType {
  val value:Any
}

trait CompositeType extends DataType {

  case class Field(n:String, dataType: Class[_<:AtomicType])
  val bindings:Map[String, Class[_<:AtomicType]]
}

trait SensorDataType extends CompositeType {
  def getNameField:Field
  def getObservationField:Field
  def getTimeField:Field

}
/**
 * Represent Integer values
 */
case class IntegerType(value:Int) extends AtomicType {
  override val name: String = "IntegerType"
}

/**
 * Represent Double values
 */
case class DoubleType(value:Double) extends AtomicType {
  override val name: String = "DoubleType"
}

/**
 * Represent Long values
 */
case class LongType(value:Long) extends AtomicType {
  override val name: String = "LongType"
}

/**
 * Represent String values
 */
case class StringType(value:String) extends AtomicType {
  override val name: String = "StringType"
}

/**
 * Represent SmartCampus Sensor Data
 */
case class SmartCampusType(value:(StringType, IntegerType, LongType) = (StringType(""), IntegerType(0), LongType(0L))) extends SensorDataType {

  val bindings = Map("n" -> classOf[StringType],
    "v" -> classOf[IntegerType],
    "t" -> classOf[LongType])

  override def getNameField: Field = Field("n", bindings("n"))

  override def getTimeField: Field = Field("t", bindings("t"))

  override def getObservationField: Field = Field("v", bindings("v"))

  override val name: String = "SmartCampusType"
}




/**
 * Represent Santander Parking Sensor Data
 */
case class SantanderParkingType(value:(IntegerType, StringType, DoubleType, IntegerType) = (IntegerType(0), StringType(""), DoubleType(0), IntegerType(0))) extends SensorDataType {
  val bindings = Map("nodeId" -> classOf[IntegerType],
                     "date" -> classOf[StringType],
                     "battery" -> classOf[DoubleType],
                     "status" -> classOf[IntegerType])

  override def getNameField: Field = Field("nodeId", classOf[IntegerType])

  override def getTimeField: Field = Field("date", classOf[StringType])

  override def getObservationField: Field = Field("status", classOf[IntegerType])

  override val name: String = "SantanderParkingType"
}

case class IntegerSensorType(value:(IntegerType, LongType) = (IntegerType(0), LongType(0L))) extends SensorDataType {
  val bindings = Map("v" -> classOf[IntegerType],
                     "t" -> classOf[LongType])

  override def getNameField: Field = throw new Exception("No name value")

  override def getTimeField: Field = Field("t", classOf[LongType])

  override def getObservationField: Field = Field("v", classOf[IntegerType])

  override val name: String = "IntegerSensorType"
}
object DataType {
  def factory(name:String) = name match {
    case "SmartCampusType" => SmartCampusType()
    case "SantanderParkingType" => SantanderParkingType()
    case "IntegerSensorType" => IntegerSensorType()
    case "IntegerType" => IntegerType(0)
    case "LongType" => LongType(0)
    case "StringType" => StringType("")
    case "DoubleType" => DoubleType(0)
    case _ => throw new Exception("Unknown data type")
  }

  def factory(name:String, values:Map[String, AtomicType]) = name match {
    case "SmartCampusType" => SmartCampusType((values("n").asInstanceOf[StringType], values("v").asInstanceOf[IntegerType], values("t").asInstanceOf[LongType]))
    case "SantanderParkingType" => SantanderParkingType((values("nodeId").asInstanceOf[IntegerType], values("date").asInstanceOf[StringType], values("batterie").asInstanceOf[DoubleType], values("status").asInstanceOf[IntegerType]))
    case "IntegerSensorType" => IntegerSensorType(values("v").asInstanceOf[IntegerType], values("t").asInstanceOf[LongType])
    case "IntegerType" => IntegerType(0)
    case "LongType" => LongType(0)
    case "StringType" => StringType("")
    case "DoubleType" => DoubleType(0)
    case _ => throw new Exception("Unknown data type")
  }

}

object DataField extends Enumeration {
  type DataField = Value
  val OBSERVATION, TIME, NAME = Value
}