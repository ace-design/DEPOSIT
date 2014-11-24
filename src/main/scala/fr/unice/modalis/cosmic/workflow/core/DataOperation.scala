package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.actions.guard.GuardAction

/**
 * Workflow data operation trait
 * Represents operations performed on data
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataOperation[T<: DataType] extends Component[T]{


def toString:String

}

/**
 * Synchronize data coming from different components
 * Pre-condition: inputs.size == outputs.size, offset > 0
 * @param inputs Inputs list
 * @param outputs Outputs list
 * @param offset Window (in ms) in which data are considered to be timed equivalent
 */
case class Synchronizer[T<: DataType](inputs:List[Input[T]], outputs:List[Output[T]], offset:Int) extends DataOperation[T]{
  require(inputs.size == outputs.size)
  require(offset > 0)

  override def toString:String = "Synchronizer[offset=" + offset + "]"

}

/**
 * Apply a transformation operation on data
 * @param transformation Transformation function
 */
case class Transformer[T<: DataType](transformation:T=>T) extends DataOperation[T] {
  val inputs = List(new Input[T](this))
  val outputs = List(new Output[T](this))

  override def toString:String = "Transformer"


}

/**
 * Assess a predicate on data
 * @param predicate Predicate to satisfy
 */
case class Predicate[T<: DataType](predicate:GuardAction) extends DataOperation[T]{
  val inputs = List(new Input[T](this))
  val outputs = List(new Output[T](this, "true"), new Output[T](this, "false"))

  val input = inputs.head

  val trueOutput = outputs.head
  val falseOutput = outputs.last

  override def toString:String = "Predicate[" + predicate + "]"


}


/**
 * Get data periodically
 * Pre-condition : period > 0
 * @param period Data collection period
 */
case class PeriodicGetter[T<: DataType](period:Int) extends DataOperation[T] {
  require(period>0)
  val inputs = List(new Input[T](this))
  val outputs = List(new Output[T](this))

  val input = inputs.head
  val output = outputs.head

  override def toString:String = "PeriodicGetter[period=" + period + "]"

}

