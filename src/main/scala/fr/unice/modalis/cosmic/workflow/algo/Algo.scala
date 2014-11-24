package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.algo.vm.{AddLink, AddElement, Instruction}
import fr.unice.modalis.cosmic.workflow.core._

import scala.collection.mutable
import scala.collection.mutable.{Stack, HashMap}

/**
 * Algorithms on Workflows
 * Created by Cyril Cecchinel - I3S Laboratory on 12/11/14.
 */
object Algo {


  def merge[T<:DataType](wf1:Workflow[T], wf2:Workflow[T]):List[Instruction[T]] = {

    def translate[T<:DataType](e:WFElement[T], dic:List[WFElement[T]]):WFElement[T] = {
      dic match {
        case a :: l => if (a.toString == e.toString) a else translate(e, l)
        case Nil => throw new NoSuchElementException

      }
    }

    def buildLink[T<:DataType](l:WFLink[T], dic:List[WFElement[T]]): WFLink[T] ={
      val newSource = translate(l.source, dic)
      val newDestination = translate(l.destination, dic)

      newSource match {
        case Predicate(_) =>  if (l.source_output.name == "true") new WFLink[T](newSource.outputs.head, newDestination.inputs.head) else new WFLink[T](newSource.outputs.last, newDestination.inputs.head)
        case _ => new WFLink[T](newSource.outputs.head, newDestination.inputs.head)
      }






    }
    var elements = List[Instruction[T]]()
    var links = List[Instruction[T]]()
    // Step 1: Workflow topological sorting
    val (w1sort, w2sort) = (sortWF(wf1), sortWF(wf2))

    // Step 2: List of merged elements
    val elements_merged = Set.empty ++ (w1sort, w2sort).zipped.map(_ + _).flatten
    // Step 3: Add instructions to build merged elements
    elements_merged.foreach(p => elements = new AddElement[T](p) :: elements)

    // Step 4 : Merge links
    val links_merged_origin = (wf1.links ++ wf2.links)



    links_merged_origin.foreach(l => links = new AddLink[T](buildLink(l, elements_merged.toList)) :: links)


    // Return list of instructions
    elements ++ links



  }


  /**
   * Sort a workflow with Topological sorting
   * @param wf A valid workflow
   * @tparam T Data type
   * @return A List containing all workflow elements.
   *         If u is before v then u and v are not connected or there exists a path from u to v
   */
  def sortWF[T<:DataType](wf:Workflow[T]) = new TopologicalSorting[T](wf).browse()


  /**
   * Inner class useful for Topological Sorting
   * Scala adaptation of http://users.polytech.unice.fr/~gaetano/asd/html/TriTopologique.html
   * @param wf Workflow to sort
   * @tparam T Data type
   */
  class TopologicalSorting[T<:DataType](wf:Workflow[T]) {


    val visited = new HashMap[WFElement[T], Int]() // Save visited elements (0 = non visited,
                                                   // 1 = currently being visited,
                                                   // 2 = visited)


    /**
     * Topological sorting
     * @return A List containing all workflow elements.
     *         If u is before v then u and v are not connected or there exists a path from u to v
     */
    def browse():List[WFElement[T]] = {
      val stack = new mutable.Stack[WFElement[T]]() // Empty stack
      for (el:WFElement[T] <- wf.elements) // Set non visited attribute to all WF elements
        visited.put(el, 0)
      for (el:WFElement[T] <- wf.elements) { // For each element, if non visited then visit
        if (visited.get(el) match {case Some(0) => true; case _ => false})
          visit(el, stack)
      }

      stack.foldLeft(List[WFElement[T]]()){(l, e) => e :: l}
    }

    /**
     * Visit the subgraph of vertex s in depth and pushes all the vertices of the sub workflow rooted
     * at s in the stack in the reverse order of the topological order.
     * Pre-condition : e has not been visited yet
     * @param e Workflow element
     * @param stack Stack
     */
    def visit(e:WFElement[T], stack:Stack[WFElement[T]]):Unit = {
      require(visited.get(e) match {case Some(0) => true; case _ => false})
      visited.update(e, 1) // Set being visited attribute
      for (el:WFElement[T] <- wf.nextElements(e)) { // For each non visited neighbour,
        if (visited.get(el) match {case Some(0) => true; case _ => false}){
          visit(el, stack) // Visit neighbour
        }
      }
      visited.update(e, 2) // Set visited attribute
      stack.push(e) // Push current WF element in stack

    }
  }
}
