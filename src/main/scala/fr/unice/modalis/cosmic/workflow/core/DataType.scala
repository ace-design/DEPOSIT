package fr.unice.modalis.cosmic.workflow.core

/**
 * Data Type trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataType {
  val value:Any
}

/**
 * Represent Integer values
 */
class IntegerType(val value:Int) extends DataType

/**
 * Represent Double values
 */
class DoubleType(val value:Double) extends DataType

/**
 * Repesent Long values
 */
class LongType(val value:Long) extends DataType

/**
 * Represent String values
 */
class StringType(val value:String) extends DataType