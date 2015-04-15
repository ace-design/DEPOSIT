package fr.unice.modalis.cosmic.workflow.core
import scala.collection.immutable.Set
/**
 * Workflow data operation trait
 * Represents operations performed on data
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Activity[I<:DataType, O<:DataType] extends Element{

  val inputsNames:Set[String]
  val outputsNames:Set[String]

  lazy val inputs = inputsNames.foldLeft(Set[Input[I]]()){(acc, e) => acc + new Input[I](e, this)}
  lazy val outputs = outputsNames.foldLeft(Set[Output[O]]()){(acc, e) => acc + new Output[O](e, this)}

  /**
   * Find an input with its name
   * @param s Requested name
   * @return Input option
   */
  def getInput(s:String):Input[I] = inputs.find(_.name.equalsIgnoreCase(s)).getOrElse(new Input[I]("default", this))

  /**
   * Find an output with its name
   * @param s Requested name
   * @return Output option
   */
  def getOutput(s:String):Output[O] = outputs.find(_.name.equalsIgnoreCase(s)).getOrElse(new Output[O]("default", this))

  override val id:String = "act" + scala.util.Random.alphanumeric.take(5).mkString



  override def toString:String

}

case class Constant[T<:DataType](val v:T) extends Activity[T,T] {
  override val inputsNames: Set[String] = Set()
  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "CONSTANT(" + v.value + ")"


}

trait Arithmetic[T<:AtomicType] extends Activity[T, T]
trait Filtering[T<:DataType] extends Activity[T, T]
trait Comparison[T<:DataType] extends Activity[T, T]

case class Extract[I<:CompositeType, O<:AtomicType](val field:String) extends Activity[I,O] {
  override val inputsNames: Set[String] = Set("input")
  override val outputsNames: Set[String] = Set("output")

  lazy val input = getInput("input")
  lazy val output = getOutput("output")

  override def toString:String = "EXTRACT(" + field + ")"

}

case class Synchronize[T<:CompositeType](val offset:Integer, val inputsNames:Set[String]) extends Activity[T, T] {
  def this(inputsNames:Set[String]) = this(0, inputsNames)

  override lazy val outputsNames: Set[String] = inputsNames

  override def toString:String = "SYNCHRONIZE"

}

/*** FILTERING OPERATIONS ***/

case class Filter[T<:DataType](val predicate:String) extends Filtering[T] {
  override val inputsNames: Set[String] = Set("input")
  override val outputsNames: Set[String] = Set("output")
  
  lazy val input = getInput("input")
  lazy val output = getOutput("output")

  override def toString:String = "FILTER(p={"+predicate+"})"

}


case class Conditional[T<:DataType](val predicate:String) extends Filtering[T] {
  override val inputsNames: Set[String] = Set("input")
  override val outputsNames: Set[String] = Set("then", "else")

  lazy val input = getInput("input")
  lazy val elseOutput = getOutput("else")
  lazy val thenOutput = getOutput("then")

  override def toString:String = "CONDITIONAL(p={"+predicate+"})"

}
/*** COMPARISON OPERATIONS ***/

case class Max[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "MAX"

}

case class Min[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "MIN"

}

case class Lower[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "LOWER"

}

case class LowerEq[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "LOWEREQ"

}

case class Higher[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "HIGHER"

}

case class HigherEq[T<:DataType](val inputsNames:Set[String]) extends Comparison[T] {
  override val outputsNames: Set[String] = Set("output")
  lazy val output = getOutput("output")

  override def toString:String = "HIGHEREQ"

}

/*** ARITHMETIC OPERATIONS ***/
case class Add[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "ADD"

}

case class Sub[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "SUB"

}

case class Multiply[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "MULTIPLY"

}

case class Divide[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "DIVIDE"
}

case class Average[I<:AtomicType](val inputsNames:Set[String]) extends Arithmetic[I] {
  def this() = this(Set())

  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output")

  override def toString:String = "AVERAGE"

}


case class Process[I<:DataType, O<:DataType](val workflow:Workflow) extends Activity[I,O]{

  override val inputsNames: Set[String] = workflow.sources.map(_.url)
  override val outputsNames: Set[String] = workflow.collectors.map(_.endpoint)

  override def toString:String = "PROCESS[" + workflow.name + "]"

}