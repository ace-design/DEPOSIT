package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core.{Source, WFElement, DataType, Workflow}

import scala.collection.mutable
import scala.collection.mutable.{Stack, HashMap}

/**
 * Algorithms on Workflows
 * Created by Cyril Cecchinel - I3S Laboratory on 12/11/14.
 */
object Algo {

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
        if (visited.get(el) == 0){
          visit(el, stack) // Visit neighbour
        }
      }
      visited.update(e, 2) // Set visited attribute
      stack.push(e) // Push current WF element in stack

    }
  }
}
