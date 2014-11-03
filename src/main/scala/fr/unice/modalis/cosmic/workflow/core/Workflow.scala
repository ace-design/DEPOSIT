package fr.unice.modalis.cosmic.workflow.core

/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param elements Workflow element list
 * @param links Link list
 * @tparam T Workflow data type
 */
case class Workflow[T <: DataType](val elements:List[WFElement], val links:List[Link[T]]) {

  def this() = this(List(), List())
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addElement(c:WFElement) = new Workflow(c :: elements, links)

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:Link[T]) = new Workflow[T](elements, l::links)

  /**
   * Workflow sources
   */
  val sources:List[Source[T]] = elements collect {case x:Source[T] => x} match {
    case l:List[Source[T]] => l
    case _ => throw new ClassCastException
  }

  /**
   * Workflow sinks
   */
  val sinks:List[Sink[T]] = elements collect {case x:Sink[T] => x} match {
    case l:List[Sink[T]] => l
    case _ => throw new ClassCastException
  }

}
