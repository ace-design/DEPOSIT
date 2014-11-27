package fr.unice.modalis.cosmic.workflow.core

/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param elements Workflow element list
 * @param links Link list
 * @tparam T Workflow data type
 */
case class Workflow[T <: DataType](val elements:Set[WFElement[T]], val links:Set[WFLink[T]]) {

  def this() = this(Set(), Set())
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addElement(c:WFElement[T]):Workflow[T]  = new Workflow[T](elements + c, links)

  /**
   * Delete an element in the current workflow (/!\ Delete also all links refering this element)
   * @param c Workflow element
   * @return A workflow without this element and links refering this element
   */
  def deleteElement(c:WFElement[T]):Workflow[T] = new Workflow[T](elements - c, links.filterNot(p => (p.destination == c) || (p.source == c)))

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink[T]):Workflow[T]  = new Workflow[T](elements, links + l)

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink[T]):Workflow[T]  = new Workflow[T](elements, links - l)

  /**
   * Find the next workflow elements"
   * @param e Current workflow element
   * @return Immediate next elements
   */
  def nextElements(e:WFElement[T]):Set[(WFElement[T], WFLink[T])] = {
    var res = Set[(WFElement[T], WFLink[T])]()
    for (l <- links) {
      if (l.source == e)
        res = res ++ links.filter(l => l.source == e).foldLeft(Set[(WFElement[T], WFLink[T])]()) { (acc, e) => acc.+((e.destination, l))}
    }

    res
  }


  // links.filter(l => l.source == e).foldLeft(Set[WFElement[T]]()){(acc, e) => acc + e.destination } // We use a set because we don't care of the order

  /**
   * Workflow sources
   */
  val sources:List[Source[T]] = elements.toList collect {case x:Source[T] => x} match {
    case l:List[Source[T]] => l
    case _ => throw new ClassCastException
  }

  /**
   * Workflow sinks
   */
  val sinks:List[Sink[T]] = elements.toList collect {case x:Sink[T] => x} match {
    case l:List[Sink[T]] => l
    case _ => throw new ClassCastException
  }

}
