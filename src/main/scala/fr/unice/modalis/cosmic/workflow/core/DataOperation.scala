package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.actions.guard.GuardAction
import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException

/**
 * Workflow data operation trait
 * Represents operations performed on data
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait DataOperation extends Component {

  val inputs: List[Input[_ <: DataType]]
  val outputs: List[Output[_ <: DataType]]

  /**
   * Find an input with its name
   * @param s Requested name
   * @return Input option
   */
  def getInput(s: String): Option[Input[_ <: DataType]] = inputs.find(_.name.equalsIgnoreCase(s))

  /**
   * Find an output with its name
   * @param s Requested name
   * @return Output option
   */
  def getOutput(s: String): Option[Output[_ <: DataType]] = outputs.find(_.name.equalsIgnoreCase(s))
}

trait IFilter extends DataOperation {
  val p: GuardAction

}

case class IntegerFilter(val p: GuardAction) extends IFilter {
  override val inputs: List[Input[IntegerType]] = List(new Input(this))
  override val outputs: List[Output[IntegerType]] = List(new Output(this))

  val input: Input[IntegerType] = inputs.head
  val output: Output[IntegerType] = outputs.head

  def ~(x: WFElement): Boolean = x.isInstanceOf[IntegerFilter] && x.asInstanceOf[IntegerFilter].p.equals(p)

  override def toString: String = "IntegerFilter_" + id


  override def +(e: WFElement): WFElement = if (e ~ this) new IntegerFilter(p) else throw new NonMergeableException


}

case class IntegerLongFilter(val p: GuardAction) extends IFilter {
  override val inputs: List[Input[_ <: DataType]] = List(new Input[LongType](this), new Input[IntegerType](this))
  override val outputs: List[Output[_ <: DataType]] = List(new Output[IntegerType](this))

  val integerInput: Input[IntegerType] = (inputs collect { case x: Input[IntegerType] => x}).head
  val longInput: Input[LongType] = (inputs collect { case x: Input[LongType] => x}).head

  val output: Output[IntegerType] = (outputs collect { case x: Output[IntegerType] => x}).head

  def ~(x: WFElement): Boolean = x.isInstanceOf[IntegerLongFilter] && x.asInstanceOf[IntegerLongFilter].p.equals(p)

  override def toString: String = "IntegerLongFilter_" + id

  override def +(e: WFElement): WFElement = if (e ~ this) new IntegerLongFilter(p) else throw new NonMergeableException

}
