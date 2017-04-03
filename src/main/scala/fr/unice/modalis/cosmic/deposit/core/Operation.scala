package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deposit.core.DataField.DataField

import scala.collection.immutable.Set
import scala.collection.mutable.ArrayBuffer

/**
 * Business concerns
 * Represents domain expert business operations
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Operation[I<:DataType, O<:DataType] extends Concept with Properties{

  val inputsNames:Set[String]
  val outputsNames:Set[String]

  final val DEFAULT_INPUT_NAME = "input"
  final val DEFAULT_OUTPUT_NAME = "output"

  lazy val inputs = inputsNames.foldLeft(Set[Input[I]]()){(acc, e) => acc + new Input[I](e, this, iType)}
  lazy val outputs = outputsNames.foldLeft(Set[Output[O]]()){(acc, e) => acc + new Output[O](e, this, oType)}

  val iType:Class[I]
  val oType:Class[O]



  /**
   * Get an input according its name
   * @param s Requested name
   * @return Input option
   */
  def getInput(s:String = DEFAULT_INPUT_NAME):Input[I] = inputs.find(_.name.equalsIgnoreCase(s)).getOrElse(throw new Exception("Input " + s +  " not found"))

  /**
   * Find an output with its name
   * @param s Requested name
   * @return Output option
   */
  def getOutput(s:String = DEFAULT_OUTPUT_NAME):Output[O] = outputs.find(_.name.equalsIgnoreCase(s)).getOrElse(throw new Exception("Output " + s +  " not found"))

  override val id:String = "concept_" + super.id

}


/**
 * Operation on values
 * @tparam T DataType
 */
trait ValueOperation[T<:DataType] extends Operation[T,T] {
  val applicationField:DataField
  val hasNewTimestamp:Boolean
  val hasNewName:Boolean
}

case class Rename[I<:DataType](newName:String, iType : Class[I]) extends Operation[I, I] {
  override val inputsNames: Predef.Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Predef.Set[String] = Set(DEFAULT_OUTPUT_NAME)
  override val oType: Class[I] = iType


  val input = getInput()
  val output = getOutput()
  /**
    * Return a copy of this concept (with different id)
    * @return copy of this concept
    */
  override def duplicate: Concept = this.copy()

  override val commonName: String = "RENAME(" + newName + ")"
}



/**
 * Merge atomic types to a given composite type
 * @param to Targeted composite type
 */
case class Merge[I<:AtomicType, O<:CompositeType](to:Class[O], iType:Class[I]) extends Operation[I,O] {
  override val inputsNames: Set[String] = DataType.factory(oType.getSimpleName).asInstanceOf[CompositeType].bindings.keySet
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val oType = to
  val output = getOutput(DEFAULT_OUTPUT_NAME)

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Merge[I,O] = this.copy(to)

  override val commonName: String = "Merge[to=" + to.getSimpleName + "]"
}

/**
 * Define a constant
 * @param v Constant
 * @tparam T Constant DataType
 */
case class Constant[T<:AtomicType](v:T, iType:Class[T]) extends Operation[T,T] {
  override val inputsNames: Set[String] = Set()
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  val oType = iType
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "CONSTANT(" + v.value + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Constant[T] = new Constant[T](v, iType)
}


/**
 * Filtering a composite type for isolating one of its fields
 * @param field Field to isolate
 * @tparam I CompositeType
 * @tparam O Type of the field to isolate
 */
case class Extract[I<:CompositeType, O<:AtomicType](field:String, iType:Class[I], oType:Class[O]) extends Operation[I,O] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val input = getInput(DEFAULT_INPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "EXTRACT(" + field + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Extract[I,O] = new Extract[I,O](field, iType, oType)
}

trait Arithmetic[T<:DataType] extends ValueOperation[T] {
  val inputsNames:Set[String]
  val rename:Option[String]
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  val oType = iType

  val applicationField = DataField.OBSERVATION
  val hasNewName  = true
  val hasNewTimestamp = true

}

trait Filtering[T<:DataType] extends ValueOperation[T] {
  val oType = iType
  val hasNewName  = false
  val hasNewTimestamp = false
}

trait Comparison[T<:DataType] extends ValueOperation[T] {
  val oType = iType
  val hasNewName  = false
  val hasNewTimestamp = false
}

case class Abs[T<:DataType](iType:Class[T], rename:Option[String] = None) extends ValueOperation[T]{
  override val applicationField: DataField = DataField.OBSERVATION
  override val hasNewName: Boolean = true
  override val hasNewTimestamp: Boolean = false
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val oType: Class[T] = iType

  /**
    * Return a copy of this concept (with different id)
    *
    * @return copy of this concept
    */
  override def duplicate: Concept = new Abs[T](iType, rename)

  override val commonName: String = "ABS"
}

case class Increment[D<:AtomicType,T<:DataType](value:D, iType:Class[T], rename:Option[String] = None) extends Arithmetic[T] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  lazy val input = getInput()

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Increment[D,T] = new Increment[D,T](value, iType, rename)

  override val commonName: String = "INCREMENT(" + value + ")"
}

case class Multiply[D<:AtomicType, T<:DataType](value:D, iType:Class[T], rename:Option[String] = None) extends Arithmetic[T] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  lazy val input = getInput()

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Multiply[D,T] = new Multiply[D,T](value, iType, rename)

  override val commonName: String = "MULTIPLY(" + value + ")"
}
case class Divide[D<:AtomicType,T<:DataType](value:D, iType:Class[T], rename:Option[String] = None) extends Arithmetic[T] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  lazy val input = getInput()

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Divide[D,T] = new Divide[D,T](value, iType, rename)

  override val commonName: String = "DIVIDE(" + value + ")"
}


/*** FILTERING OPERATIONS ***/


/**
 * Filter on-the-fly data according a predicate
 * @param predicate Predicate
 * @tparam T DataType
 */
case class Conditional[T<:DataType](predicate:String, iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends Filtering[T] {
  final val THEN_OUTPUT_NAME = "then"
  final val ELSE_OUTPUT_NAME = "else"

  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Set[String] = Set(THEN_OUTPUT_NAME, ELSE_OUTPUT_NAME)

  lazy val input = getInput(DEFAULT_INPUT_NAME)
  lazy val elseOutput = getOutput(ELSE_OUTPUT_NAME)
  lazy val thenOutput = getOutput(THEN_OUTPUT_NAME)
  override val commonName: String = "CONDITIONAL(" + predicate + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Conditional[T] = new Conditional[T](predicate, iType)
}

case class Produce[I<:DataType, O<:DataType](inputsNames:Set[String], onSuccess:O, onFailure:Option[O], iType:Class[I], oType:Class[O]) extends Operation[I,O] {
  override val outputsNames: Predef.Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Produce[I,O] = new Produce[I,O](inputsNames, onSuccess, onFailure, iType, oType)

  override val commonName: String = "PRODUCE[" + onSuccess + "/" + onFailure + "]"
}

/*** COMPARISON OPERATIONS ***/

/**
 * Find the maximum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Max[T<:DataType](inputsNames:Set[String], iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "MAX"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Max[T] = new Max[T](inputsNames, iType)
}

/**
 * Find the minimum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Min[T<:DataType](inputsNames:Set[String], iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "MIN"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Min[T] = new Min[T](inputsNames, iType)
}


/**
 * Binary comparisons
 * @tparam T DataType
 */
trait BinaryComparison[T<:DataType] extends Comparison[T] {
  final def LEFT_OPERAND_NAME = "i1"
  final def RIGHT_OPERAND_NAME = "i2"
  override val inputsNames = Set(LEFT_OPERAND_NAME, RIGHT_OPERAND_NAME)
  override val outputsNames = Set(DEFAULT_OUTPUT_NAME)

  lazy val leftOperand = getInput(LEFT_OPERAND_NAME)
  lazy val rightOperand = getInput(RIGHT_OPERAND_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)


}

case class Lower[T<:DataType](iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends BinaryComparison[T] {
  override val commonName: String = "LOWER"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Lower[T] = new Lower[T](iType)
}

case class LowerEq[T<:DataType](iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends BinaryComparison[T] {
  override val commonName: String = "LOWEREQ"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: LowerEq[T] = new LowerEq[T](iType)
}

case class Higher[T<:DataType](iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends BinaryComparison[T] {
  override val commonName: String = "HIGHER"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Higher[T] = new Higher[T](iType)
}

case class HigherEq[T<:DataType](iType:Class[T], applicationField:DataField = DataField.OBSERVATION) extends BinaryComparison[T] {
  override val commonName: String = "HIGHEREQ"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: HigherEq[T] = new HigherEq[T](iType)
}

/*** ARITHMETIC OPERATIONS ***/
case class Add[I<:DataType](inputsNames:Set[String], iType:Class[I], rename:Option[String] = None) extends Arithmetic[I] {
  override val commonName: String = "ADD"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Add[I] = new Add[I](inputsNames, iType, rename)
}

case class Sub[I<:DataType](inputsNames:Set[String], iType:Class[I], rename:Option[String] = None) extends Arithmetic[I] {
  override val commonName: String = "SUB"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Sub[I] = new Sub[I](inputsNames, iType, rename)
}

case class Average[I<:DataType](inputsNames:Set[String], iType:Class[I], rename:Option[String] = None) extends Arithmetic[I] {
  override val commonName: String = "AVERAGE"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Average[I] = new Average[I](inputsNames, iType, rename)
}

/*** PROCESS OPERATION ***/
case class Process[I<:DataType, O<:DataType](workflow:Policy, iType:Class[I], oType:Class[O]) extends Operation[I,O]{

  override val commonName: String = "PROCESS(" + workflow.name + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Process[I,O] = new Process[I,O](workflow, iType, oType)

  override val inputsNames: Set[String] = workflow.sources.map(_.name)
  override val outputsNames: Set[String] = workflow.collectors.map(_.name)


  def expand(parent:Policy) = {
    if (!parent.operations.contains(this)) throw new NotExpendableException(parent)

    // Duplicate the inner workflow (re-generating the concept id)
    val _workflow = workflow.duplicate

    val newFlows = ArrayBuffer[Flow[_<:DataType]]()
    // Compute the links to add between the previous concept and the first concept of this process
    parent.flowsTo(this).foreach {
      l =>
        _workflow.flowsFrom(
          _workflow.ios.find(_.name == l.destination_input.name).get)
          .foreach(n => newFlows += Flow(l.source_output.asInstanceOf[Output[_<:DataType]], n.destination_input.asInstanceOf[Input[_<:DataType]]))
    }

    // Compute the links to add between the last concept of this process and the first concept after this process
    parent.flowsFrom(this).foreach {
      l =>
        _workflow.flowsTo(
          _workflow.ios.find(_.name == l.source_output.name).get)
          .foreach(n => newFlows += Flow(n.source_output.asInstanceOf[Output[_<:DataType]], l.destination_input.asInstanceOf[Input[_<:DataType]])
        )
    }

    // Delete all input/output concepts in the embedded workflow
    var resultIODeletion = _workflow
    _workflow.ios.foreach {io => resultIODeletion = resultIODeletion.delete(io)}

    var transformationsResult = parent
    // Delete the process concept into the parent policy
    transformationsResult = transformationsResult.delete(this)

    // Add the operations and links into the parent policy
    resultIODeletion.operations.foreach(o => transformationsResult = transformationsResult.add(o))
    resultIODeletion.flows.foreach(l => transformationsResult = transformationsResult.addFlow(l))
    newFlows.foreach(l => transformationsResult = transformationsResult.addFlow(l))

    // Return the policy
    transformationsResult
  }


}

