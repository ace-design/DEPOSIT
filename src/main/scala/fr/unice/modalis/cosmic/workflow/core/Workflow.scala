package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.Algo
import fr.unice.modalis.cosmic.workflow.algo.vm.{Instruction, Merge, VirtualMachine}

import scala.collection.mutable.ArrayBuffer

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
   * Delete an element in the current workflow (/!\ Delete also all links referring this element)
   * @param c Workflow element
   * @return A workflow without this element and links referring this element
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

  /**
   * Return sub Workflow
   * @param root Root element
   */
  def subWorkflow(root:WFElement):Workflow = {
    val elements = new ArrayBuffer[WFElement]()
    val links = new ArrayBuffer[WFLink]()

    // Add root into the elements
    elements += root
    def internal(e:WFElement):Unit = {
      val next = nextElements(e)
      next.foreach(e => {elements += e._1; links += e._2; if (e != Set.empty) internal(e._1)})

    }
    internal(root)
    new Workflow(elements.toSet, links.toSet)
  }

  /**
   * Merge two workflows
   * @param other Workflow to be merged with
   * @return Merged workflow
   */
  def +(other: Workflow) = {

    def mergeInternal(l:List[WFElement]):List[Instruction] = {
      l.sortWith(_ ~ _) match {
        case a :: b :: l if a ~ b => Merge(a,b) :: mergeInternal(l)
        case a :: b :: l => mergeInternal(b :: l)
        case a => Nil
      }
    }
    // Compute the actions needed to merge the two workflows
    val actions = Algo.merge(this, other)

    // Apply the actions and produce a new workflow
    var intermediateWF = VirtualMachine(this, actions)

    // Identify if similar elements can be merged into the workflow
    var needMerging = Algo.similar(intermediateWF)

    // Loop until there is no more possibility of merging
    while(needMerging != Set.empty){
      val actions = mergeInternal(needMerging.toList)
      intermediateWF = VirtualMachine(intermediateWF, actions)
      needMerging = Algo.similar(intermediateWF)
    }

    intermediateWF

  }
  
  def ~(other: Workflow) = {
    Algo.similar(other) == Set.empty
  }
}
