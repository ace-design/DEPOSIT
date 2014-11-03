package fr.unice.modalis.cosmic.workflow.core

/**
 * Data Type trait
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataType {

}

/**
 * Represent Integer values
 */
class IntegerType(val value:Int) extends DataType

/**
 * Represent Double values
 */
class DoubleType(val value:Double) extends DataType