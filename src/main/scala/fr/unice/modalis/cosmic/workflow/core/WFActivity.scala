package fr.unice.modalis.cosmic.workflow.core
import scala.collection.immutable.Set
/**
 * Workflow data operation trait
 * Represents operations performed on data
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFActivity[I<:DataType, O<:DataType] extends WFElement{

  val inputsNames:Set[String]
  val outputsNames:Set[String]

  lazy val inputs = inputsNames.foldLeft(Set[Input[I]]()){(acc, e) => acc + new Input[I](e, this)}
  lazy val outputs = outputsNames.foldLeft(Set[Output[O]]()){(acc, e) => acc + new Output[O](e, this)}

  /**
   * Find an input with its name
   * @param s Requested name
   * @return Input option
   */
  def getInput(s:String):Option[Input[I]] = inputs.find(_.name.equalsIgnoreCase(s))

  /**
   * Find an output with its name
   * @param s Requested name
   * @return Output option
   */
  def getOutput(s:String):Option[Output[O]] = outputs.find(_.name.equalsIgnoreCase(s))

  override val id:String = "act" + scala.util.Random.alphanumeric.take(5).mkString



  override def toString:String

}


case class Script[I<:DataType, O<:DataType](val pathToScript:String, val inputsNames:Set[String], val outputsNames:Set[String]) extends WFActivity[I,O] {
  def this(pathToScript:String) = this(pathToScript, Set(), Set())
  override def toString:String = "SCRIPT[" + pathToScript + "]"

}

case class IfThenElse[I<:DataType, O<:DataType](val ifStatement: String, val thenStatement:String, val elseStatement:String, val inputsNames:Set[String], val outputsNames:Set[String]) extends WFActivity[I,O] {
  def this(ifStatement:String, thenStatement:String, elseStatement:String) = this(ifStatement, thenStatement, elseStatement, Set(), Set())
  override def toString:String = "IFTHENELSE"

}

case class Average[I<:DataType](val inputsNames:Set[String]) extends WFActivity[I,I] {
  def this() = this(Set())

  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output").get

  override def toString:String = "AVERAGE"

}

case class Cast[I<:DataType, O<:DataType]() extends WFActivity[I, O] {
  override val inputsNames: Set[String] = Set("input")
  override val outputsNames: Set[String] = Set("output")

  lazy val input = getInput("input").get
  lazy val output = getOutput("output").get

  override def toString:String = "CAST"
}

case class Min[I<:DataType](val inputsNames:Set[String]) extends WFActivity[I,I] {
  def this() = this(Set())

  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output").get

  override def toString:String = "MIN"

}

case class Max[I<:DataType](val inputsNames:Set[String]) extends WFActivity[I,I] {
  def this() = this(Set())

  override val outputsNames: Set[String] = Set("output")

  lazy val output = getOutput("output").get

  override def toString:String = "MAX"

}