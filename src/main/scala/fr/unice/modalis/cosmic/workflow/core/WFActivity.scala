package fr.unice.modalis.cosmic.workflow.core
import scala.collection.immutable.Set
/**
 * Workflow data operation trait
 * Represents operations performed on data
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFActivity[I<:DataType, O<:DataType] extends WFElement{

  var inputs:Set[Input[I]]
  var outputs:Set[Output[O]]


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



  override def toString:String

}


case class Script[I<:DataType, O<:DataType](val pathToScript:String, var inputs:Set[Input[I]], var outputs:Set[Output[O]]) extends WFActivity[I,O] {

  def this(pathToScript:String) = this(pathToScript, Set(), Set())



  override def toString:String = "SCRIPT[" + pathToScript + "]{" + id + "}"


}

case class IfThenElse[I<:DataType, O<:DataType](val ifStatement: String, val thenStatement:String, val elseStatement: String, var inputs:Set[Input[I]], var outputs:Set[Output[O]]) extends WFActivity[I,O] {

  def this(ifStatement:String, thenStatement:String, elseStatement:String) = this(ifStatement, thenStatement, elseStatement, Set(), Set())


  override def toString:String = "IFTHENELSE{" + id + "}"

}

case class Average[I<:DataType](var inputs:Set[Input[I]]) extends WFActivity[I,I] {

  def this() = this(Set())

  var outputs = Set(Output[I]("output"))

  val output = getOutput("output").get



  override def toString:String = "AVERAGE{" + id + "}"

}

case class Min[I<:DataType](var inputs:Set[Input[I]]) extends WFActivity[I,I] {
  def this() = this(Set())
  var outputs = Set(Output[I]("output"))
  val output = getOutput("output").get

  override def toString:String = "MIN{" + id + "}"

}

case class Max[I<:DataType](var inputs:Set[Input[I]]) extends WFActivity[I,I] {
  def this() = this(Set())
  var outputs = Set(Output[I]("output"))
  val output = getOutput("output").get


  override def toString:String = "MAX{" + id + "}"

}