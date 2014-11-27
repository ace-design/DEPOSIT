package fr.unice.modalis.cosmic.workflow.algo

import java.util.NoSuchElementException

import fr.unice.modalis.cosmic.workflow.algo.vm._
import fr.unice.modalis.cosmic.workflow.core._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack, HashMap}

/**
 * Algorithms on Workflows
 * Created by Cyril Cecchinel - I3S Laboratory on 12/11/14.
 */
object Algo {


  def merge[T<:DataType](wf1:Workflow[T], wf2:Workflow[T]):List[Instruction[T]] = {

    val setActions = ArrayBuffer[Instruction[T]]()
    val newElements = mutable.HashMap[String, WFElement[T]]() // Directory (old --> new)

    def convert(e: String):WFElement[T] = {
      newElements.get(e) match {
        case Some(a) => println("\t [ALERT] New version found for: " + e + "-->" + a.uid); a // The asked element is outdated and we provide the real element
        case None => throw new NoSuchElementException // The asked element is up to date
      }
    }

    def upToDateElement(e1: WFElement[T]):WFElement[T] = {
      try { convert(e1.uid) } catch { case _: NoSuchElementException => e1}
    }

    def merge_internal(req1: WFElement[T], req2: WFElement[T]): Unit = {
      // Update references
      val e1 = upToDateElement(req1)
      val e2 = upToDateElement(req2)

      if (e1.equals(e2)) {
        // step 1 : Merge e1 and e2 and delete e1/e2
        val res = (e1 + e2) // We are sure about e1 == e2 property


        println("[Merge] " + e1.uid + " [WF1] " + e2.uid + " [WF2] => " + res + " (" + res.uid + ")")
        println("\t* Need to add: " + res.uid)
        setActions += AddElement(res)
        println("\t* Need to delete [WF1]: " + e1.uid)
        setActions += DeleteElement(e1)
        println("\t* Need to delete [WF2]: " + e2.uid)
        setActions += DeleteElement(e2)

        // step 2a : Delete all links with e1 as source and replace by new links with res as source (in both workflow)
        setActions ++= wf1.links.filter(l => upToDateElement(l.source) == e1).foldLeft(List[Instruction[T]]()) { (acc, e) => val created = new WFLink[T](res.outputs.head, e.destination_input); println("\t* Need to delete link [WF1]: " + e); println("\t* Need to add link [WF1]: " + created); DeleteLink(e) :: AddLink(created) :: acc}
        setActions ++= wf2.links.filter(l => upToDateElement(l.source) == e2).foldLeft(List[Instruction[T]]()) { (acc, e) => val created = new WFLink[T](res.outputs.head, e.destination_input); println("\t* Need to delete link [WF2]: " + e); println("\t* Need to add link [WF2]: " + created); DeleteLink(e) :: AddLink(created) :: acc}

        // step 2b : Delete all links with e1 as destination and replace by new links with res as source (in both workflow)
        setActions ++= wf1.links.filter(l => upToDateElement(l.destination) == e1).foldLeft(List[Instruction[T]]()) { (acc, e) => val created = new WFLink[T](e.source_output, res.inputs.head); println("\t* Need to delete link [WF1]: " + e); println("\t* Need to add link [WF1]: " + created); DeleteLink(e) :: AddLink(created) :: acc}
        setActions ++= wf2.links.filter(l => upToDateElement(l.destination) == e2).foldLeft(List[Instruction[T]]()) { (acc, e) => val created = new WFLink[T](e.source_output, res.inputs.head); println("\t* Need to delete link [WF2]: " + e); println("\t* Need to add link [WF2]: " + created); DeleteLink(e) :: AddLink(created) :: acc}

        newElements += (e1.uid -> res)
        newElements += (e2.uid -> res) // We save the new created element


        // step 3 : Loop on next elements
        val nextElementWF1 = wf1.nextElements(e1).map(_._1)
        val nextElementWF2 = wf2.nextElements(e2).map(_._1)


        println("\t* Merge finished. Loop on " + nextElementWF1 + " and " + nextElementWF2)
        (nextElementWF1.toList, nextElementWF2.toList) match {
          case (a :: _, b :: _) => merge_internal(a, b)
          case (_, _) => // Finished
        }


      }
      else {
        println("[STOP] Differs from " + e1.uid + " / " + e2.uid)
      }

    }

    merge_internal(wf1.sources.head, wf2.sources.head)
    setActions.toList

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
      for (el:WFElement[T] <- wf.nextElements(e).map(_._1)) { // For each non visited neighbour,
        if (visited.get(el) match {case Some(0) => true; case _ => false}){
          visit(el, stack) // Visit neighbour
        }
      }
      visited.update(e, 2) // Set visited attribute
      stack.push(e) // Push current WF element in stack

    }
  }
}
