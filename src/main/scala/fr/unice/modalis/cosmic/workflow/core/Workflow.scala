package fr.unice.modalis.cosmic.workflow.core

/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param elements Workflow element list
 * @param links Link list

 */
case class Workflow(val elements:Set[WFElement], val links:Set[WFLink]) {

  def this() = this(Set.empty, Set.empty)
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addElement(c:WFElement):Workflow  = new Workflow(elements + c, links)

  /**
   * Delete an element in the current workflow (/!\ Delete also all links refering this element)
   * @param c Workflow element
   * @return A workflow without this element and links refering this element
   */
  def deleteElement(c:WFElement):Workflow = new Workflow(elements - c, links.filterNot(p => (p.destination == c) || (p.source == c)))

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink):Workflow  = new Workflow(elements, links + l)

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink):Workflow  = new Workflow(elements, links - l)

  /**
   * Find the next workflow elements
   * @param e Current workflow element
   * @return Immediate next elements
   */
  def nextElements(e:WFElement):Set[(WFElement, WFLink)] = {
    var res = Set[(WFElement, WFLink)]()
    for (l <- links) {
      if (l.source == e)
        res = res ++ links.filter(l => l.source == e).foldLeft(Set.empty[(WFElement, WFLink)]){(acc, e) => acc.+((e.destination, l))}
    }

    res
  }

  /**
   * Workflow sources
   */
  val sources:List[Source[DataType]] = elements.toList collect {case x:Source[DataType] => x} match {
    case l:List[Source[DataType]] => l
    case _ => throw new ClassCastException
  }

  /**
   * Workflow sinks
   */
  val sinks:List[Sink[DataType]] = elements.toList collect {case x:Sink[DataType] => x} match {
    case l:List[Sink[DataType]] => l
    case _ => throw new ClassCastException
  }

}
