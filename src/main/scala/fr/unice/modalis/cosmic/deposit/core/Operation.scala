package fr.unice.modalis.cosmic.deposit.core

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

  lazy val inputs = inputsNames.foldLeft(Set[Input[I]]()){(acc, e) => acc + new Input[I](e, this)}
  lazy val outputs = outputsNames.foldLeft(Set[Output[O]]()){(acc, e) => acc + new Output[O](e, this)}



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

/** Classification of business concerns **/
trait Arithmetic[T<:AtomicType] extends Operation[T, T] {
  val inputsNames:Set[String]

  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

}

trait Filtering[T<:DataType] extends Operation[T, T]
trait Comparison[T<:DataType] extends Operation[T, T]


/**
 * Define a constant
 * @param v Constant
 * @tparam T Constant DataType
 */
case class Constant[T<:DataType](v:T) extends Operation[T,T] {
  override val inputsNames: Set[String] = Set()
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "CONSTANT(" + v.value + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Constant[T](v)
}


/**
 * Filtering a composite type for isolating one of its fields
 * @param field Field to isolate
 * @tparam I CompositeType
 * @tparam O Type of the field to isolate
 */
case class Extract[I<:CompositeType, O<:AtomicType](field:String) extends Operation[I,O] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val input = getInput(DEFAULT_INPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "EXTRACT(" + field + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Extract[I,O](field)
}

/*** FILTERING OPERATIONS ***/


/**
 * Filter on-the-fly data according a predicate
 * @param predicate Predicate
 * @tparam T DataType
 */
case class Conditional[T<:DataType](predicate:String) extends Filtering[T] {
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
  override def duplicate: Concept = new Conditional[T](predicate)
}

case class Switch[T<:DataType](switchMap:Map[String,String], inputsNames:Set[String], outputsNames:Set[String]) extends Filtering[T] {
  require(switchMap.keys.forall(inputsNames.contains))
  require(switchMap.values.forall(outputsNames.contains))
  override val commonName: String = "SWITCH(" + switchMap + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Switch[T](switchMap, inputsNames, outputsNames)
}
/*** COMPARISON OPERATIONS ***/

/**
 * Find the maximum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Max[T<:DataType](inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "MAX"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Max[T](inputsNames)
}

/**
 * Find the minimum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Min[T<:DataType](inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)
  override val commonName: String = "MIN"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Min[T](inputsNames)
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

case class Lower[T<:DataType]() extends BinaryComparison[T] {
  override val commonName: String = "LOWER"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Lower[T]
}

case class LowerEq[T<:DataType]() extends BinaryComparison[T] {
  override val commonName: String = "LOWEREQ"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new LowerEq[T]
}

case class Higher[T<:DataType]() extends BinaryComparison[T] {
  override val commonName: String = "HIGHER"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Higher[T]
}

case class HigherEq[T<:DataType]() extends BinaryComparison[T] {
  override val commonName: String = "HIGHEREQ"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new HigherEq[T]
}

/*** ARITHMETIC OPERATIONS ***/
case class Add[I<:AtomicType](inputsNames:Set[String]) extends Arithmetic[I] {
  override val commonName: String = "ADD"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Add[I](inputsNames)
}

case class Sub[I<:AtomicType](inputsNames:Set[String]) extends Arithmetic[I] {
  override val commonName: String = "SUB"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Sub[I](inputsNames)
}

case class Multiply[I<:AtomicType](inputsNames:Set[String]) extends Arithmetic[I] {
  override val commonName: String = "MULTIPLY"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Multiply[I](inputsNames)
}

case class Divide[I<:AtomicType](inputsNames:Set[String]) extends Arithmetic[I] {
  override val commonName: String = "DIVIDE"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Divide[I](inputsNames)
}

case class Average[I<:AtomicType](inputsNames:Set[String]) extends Arithmetic[I] {
  override val commonName: String = "AVERAGE"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Average[I](inputsNames)
}

/*** PROCESS OPERATION ***/
case class Process[I<:DataType, O<:DataType](workflow:Policy) extends Operation[I,O]{

  override val commonName: String = "PROCESS(" + workflow.name + ")"

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  override def duplicate: Concept = new Process[I,O](workflow)

  override val inputsNames: Set[String] = workflow.sources.map(_.name)
  override val outputsNames: Set[String] = workflow.collectors.map(_.name)


  def expand(parent:Policy) = {
    if (!parent.operations.contains(this)) throw new NotExtendableException(parent)

    // Duplicate the inner workflow (re-generating the concept id)
    val _workflow = workflow.duplicate

    val newLinks = ArrayBuffer[Link[_<:DataType]]()
    // Compute the links to add between the previous concept and the first concept of this process
    parent.linksTo(this).foreach {
      l =>
        _workflow.linksFrom(
          _workflow.ios.find(_.name == l.destination_input.name).get)
          .foreach(n => newLinks += Link(l.source_output, n.destination_input))
    }

    // Compute the links to add between the last concept of this process and the first concept after this process
    parent.linksFrom(this).foreach {
      l =>
        _workflow.linksTo(
          _workflow.ios.find(_.name == l.source_output.name).get)
          .foreach(n => newLinks += Link(n.source_output, l.destination_input)
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
    resultIODeletion.links.foreach(l => transformationsResult = transformationsResult.addLink(l))
    newLinks.foreach(l => transformationsResult = transformationsResult.addLink(l))

    // Return the policy
    transformationsResult
  }


}

