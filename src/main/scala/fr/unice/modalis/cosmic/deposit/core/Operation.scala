package fr.unice.modalis.cosmic.deposit.core
import scala.collection.immutable.Set
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
  def getInput(s:String):Input[I] = inputs.find(_.name.equalsIgnoreCase(s)).getOrElse(throw new Exception("Input " + s +  " not found"))

  /**
   * Find an output with its name
   * @param s Requested name
   * @return Output option
   */
  def getOutput(s:String):Output[O] = outputs.find(_.name.equalsIgnoreCase(s)).getOrElse(throw new Exception("Output " + s +  " not found"))

  override val id:String = "concept_" + scala.util.Random.alphanumeric.take(5).mkString
  
  override def toString:String

}

/** Classification of business concerns **/
trait Arithmetic[T<:AtomicType] extends Operation[T, T] {
  val inputsNames:Set[String]

  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  def toString:String
}

trait Filtering[T<:DataType] extends Operation[T, T]
trait Comparison[T<:DataType] extends Operation[T, T]


/**
 * Define a constant
 * @param v Constant
 * @tparam T Constant DataType
 */
case class Constant[T<:DataType](val v:T) extends Operation[T,T] {
  override val inputsNames: Set[String] = Set()
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  override def toString:String = "CONSTANT(" + v.value + ")"

}


/**
 * Filtering a composite type for isolating one of its fields
 * @param field Field to isolate
 * @tparam I CompositeType
 * @tparam O Type of the field to isolate
 */
case class Extract[I<:CompositeType, O<:AtomicType](val field:String) extends Operation[I,O] {
  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)

  lazy val input = getInput(DEFAULT_INPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  override def toString:String = "EXTRACT(" + field + ")"

}

/*** FILTERING OPERATIONS ***/


/**
 * Filter on-the-fly data according a predicate
 * @param predicate Predicate
 * @tparam T DataType
 */
case class Conditional[T<:DataType](val predicate:String) extends Filtering[T] {
  final val THEN_OUTPUT_NAME = "then"
  final val ELSE_OUTPUT_NAME = "else"

  override val inputsNames: Set[String] = Set(DEFAULT_INPUT_NAME)
  override val outputsNames: Set[String] = Set(THEN_OUTPUT_NAME, ELSE_OUTPUT_NAME)

  lazy val input = getInput(DEFAULT_INPUT_NAME)
  lazy val elseOutput = getOutput(ELSE_OUTPUT_NAME)
  lazy val thenOutput = getOutput(THEN_OUTPUT_NAME)

  override def toString:String = "CONDITIONAL(p={"+predicate+"})"

}
/*** COMPARISON OPERATIONS ***/

/**
 * Find the maximum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Max[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  override def toString:String = "MAX"

}

/**
 * Find the minimum value on an inputs set
 * @param inputsNames Inputs names
 * @tparam T Inputs DataType
 */
case class Min[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set(DEFAULT_OUTPUT_NAME)
  lazy val output = getOutput(DEFAULT_OUTPUT_NAME)

  override def toString:String = "MIN"

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

  def toString:String
}

case class Lower[T<:DataType]() extends BinaryComparison[T] {
  override def toString:String = "LOWER"
}

case class LowerEq[T<:DataType]() extends BinaryComparison[T] {
  override def toString:String = "LOWEREQ"
}

case class Higher[T<:DataType]() extends BinaryComparison[T] {
  override def toString:String = "HIGHER"
}

case class HigherEq[T<:DataType]() extends BinaryComparison[T] {
  override def toString:String = "HIGHEREQ"
}

/*** ARITHMETIC OPERATIONS ***/
case class Add[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override def toString:String = "ADD"
}

case class Sub[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override def toString:String = "SUB"
}

case class Multiply[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override def toString:String = "MULTIPLY"
}

case class Divide[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override def toString:String = "DIVIDE"
}

case class Average[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override def toString:String = "AVERAGE"
}

/*** PROCESS OPERATION ***/
case class Process[I<:DataType, O<:DataType](val workflow:Policy) extends Operation[I,O]{
  override val inputsNames: Set[String] = workflow.sources.map(_.url)
  override val outputsNames: Set[String] = workflow.collectors.map(_.endpoint)

  override def toString:String = "PROCESS[" + workflow.name + "]"

}