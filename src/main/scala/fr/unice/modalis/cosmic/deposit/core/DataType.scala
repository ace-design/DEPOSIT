package fr.unice.modalis.cosmic.deposit.core

/**
 * Data Type trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataType {
  val value:Any
  val id:String = "type" + scala.util.Random.alphanumeric.take(5).mkString


}
trait AtomicType extends DataType {
  val name:String
}

trait CompositeType extends DataType {
  val bindings:Map[String, AtomicType]
}
/**
 * Represent Integer values
 */
class IntegerType(val value:Int) extends AtomicType {
  override val name: String = "int"
}

/**
 * Represent Double values
 */
class DoubleType(val value:Double) extends AtomicType {
  override val name: String = "double"
}

/**
 * Represent Long values
 */
class LongType(val value:Long) extends AtomicType {
  override val name: String = "long"
}

/**
 * Represent String values
 */
class StringType(val value:String) extends AtomicType {
  override val name: String = "String"
}

/**
 * Represent SmartCampus Sensor Data
 */
class SmartCampusType(val value:(String, Int, Long)) extends CompositeType {
  val bindings = Map("n" -> new StringType(value._1),
                     "v" -> new IntegerType(value._2),
                     "t" -> new LongType(value._3))
}

/**
 * Represent Santander Parking Sensor Data
 */
class SantanderParkingType(val value:(Int, String, Double, Int)) extends CompositeType {
  val bindings = Map("nodeId" -> new IntegerType(value._1),
                     "date" -> new StringType(value._2),
                     "battery" -> new DoubleType(value._3),
                     "status" -> new IntegerType(value._4))
}