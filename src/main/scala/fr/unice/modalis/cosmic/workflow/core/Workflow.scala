package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.Algo
import fr.unice.modalis.cosmic.workflow.algo.vm._

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
  def addElement(c:WFElement):Workflow  = {
    val newWF = new Workflow(elements + c, links)
    newWF.optimize
  }

  def addElementBefore(e:WFElement, before:WFElement) = {
    require(elements.contains(before))

    val actionsList = ArrayBuffer[Instruction]()
    // Retrieve all the links coming on before
    val linksToBefore = links.filter(p => p.destination == before)

    // Delete those links
    linksToBefore.foreach(actionsList += new DeleteLink(_))

    // Add new element
    actionsList += new AddElement(e)

    // Build links
    linksToBefore.map(l => new WFLink(l.source_output, e.inputs.head)).foreach(actionsList += new AddLink(_))

    linksToBefore.map(l => new WFLink(e.outputs.head, l.destination_input)).foreach(actionsList += new AddLink(_))

    // Compute new workflow
    VirtualMachine(this, actionsList.toList)
  }

  /**
   * Delete an element in the current workflow (/!\ Delete also all links referring this element)
   * @param c Workflow element
   * @return A workflow without this element and links referring this element
   */
  def deleteElement(c:WFElement):Workflow = {
    val linksToDelete = links.filter(p => (p.destination == c) || (p.source == c))
    println("Delete [" + c + "] and links:" + linksToDelete)
    val newWF = new Workflow(elements - c, links -- linksToDelete)
    newWF.optimize
  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink):Workflow  = {
    val newWF = new Workflow(elements, links + l)
    newWF.optimize
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink):Workflow  = {
    println("Delete [" + l + "]")
    val newWF = new Workflow(elements, links - l)
    newWF.optimize
  }




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
  def +(other: Workflow):Workflow = {
    println("*** Beginning merging ... ***")

    // Compute the actions needed to merge the two workflows
    val actions = Algo.merge(this, other)

    // Apply the actions and produce a new workflow
    val result = VirtualMachine(this, actions)

    println("*** End merging ... ***")

    result
  }

  def optimize():Workflow = {

    // Inner function that identify if there are elements that can be merged
    def mergeInternal(l:List[WFElement]):List[Instruction] = {
      l.sortWith(_ ~ _) match {
        case a :: b :: l if a ~ b => Merge(a,b) :: mergeInternal(l)
        case a :: b :: l => mergeInternal(b :: l)
        case a => Nil
      }
    }

    var intermediateWF = this

    println("*** Beginning optimization ... ***")
    println("[INFO] Find similar elements ...")

    // Identify if similar elements can be merged into the workflow
    var needMerging = Algo.similarElements(intermediateWF)

    // Loop until there is no more possibility of merging
    while((needMerging != Set.empty) && (needMerging.size > 1)){
      val actions = mergeInternal(needMerging.toList)
      intermediateWF = VirtualMachine(intermediateWF, actions)
      needMerging = Algo.similarElements(intermediateWF)
    }
    println("[INFO] Find similar sinks ...")
    // Merge sinks if needed
    val similarSinks = Algo.similarSinks(intermediateWF)
    val actions_sinks = similarSinks.map(e => new Merge(e._1, e._2))

    intermediateWF = VirtualMachine(intermediateWF, actions_sinks.toList)


    links.filterNot(p => p.source.isInstanceOf[ISynchronizer] || p.destination.isInstanceOf[ISynchronizer]).groupBy(l => l.destination).map(t => (t._1, t._2.size)).filter(_._2 > 1).foreach(e => println("[WARNING] You may use a synchronizer before " + e._1))

    println("*** End optimization ... ***")

    intermediateWF
  }

  /**
   * Check if this workflow is similar to an other
   * @param other Other workflow
   * @return A boolean refering if this workflow is similar to an other
   */
  def ~(other: Workflow) = {
    Algo.areSimilarWorkflows(this,other)
  }


}
