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
  def getNameField:Field
  def getObservationField:Field
  def getTimeField:Field
}
/**
 * Represent Integer values
 */
case class IntegerType(val value:Int) extends AtomicType {
  override val name: String = "IntegerType"
}

/**
 * Represent Double values
 */
case class DoubleType(val value:Double) extends AtomicType {
  override val name: String = "DoubleType"
}

/**
 * Represent Long values
 */
case class LongType(val value:Long) extends AtomicType {
  override val name: String = "LongType"
}

/**
 * Represent String values
 */
case class StringType(val value:String) extends AtomicType {
  override val name: String = "StringType"
}

/**
 * Represent SmartCampus Sensor Data
 */
case class SmartCampusType() extends CompositeType {
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
case class SantanderParkingType() extends CompositeType {
  val bindings = Map("nodeId" -> classOf[IntegerType],
                     "date" -> classOf[StringType],
                     "battery" -> classOf[DoubleType],
                     "status" -> classOf[IntegerType])

  override def getNameField: Field = Field("nodeId", classOf[IntegerType])

  override def getTimeField: Field = Field("date", classOf[StringType])

  override def getObservationField: Field = Field("status", classOf[IntegerType])

  override val name: String = "SantanderParkingType"
}

object DataType {
  def factory(name:String) = name match {
    case "SmartCampusType" => SmartCampusType()
    case "SantanderParkingType" => SantanderParkingType()
    case "IntegerType" => IntegerType(0)
    case "LongType" => LongType(0)
    case "StringType" => StringType("")
    case "DoubleType" => DoubleType(0)
    case _ => throw new Exception("Unknown data type")
  }
}