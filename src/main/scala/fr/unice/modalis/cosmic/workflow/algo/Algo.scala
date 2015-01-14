package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.algo.vm._
import fr.unice.modalis.cosmic.workflow.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Algorithms on Workflows
 * Created by Cyril Cecchinel - I3S Laboratory on 12/11/14.
 */
object Algo {

  /**
   * Check if two workflow are similar
   * @param wf1 Workflow 1
   * @param wf2 Workflow 2
   * @return A boolean : Workflow1 is similar to workflow2?
   */
  def areSimilarWorkflows(wf1:Workflow, wf2:Workflow) = {
    (wf1.links.size == wf2.links.size) && wf1.links.forall(l => wf2.links.exists(p => l.source ~ p.source && l.destination ~ p.destination))
  }

  /**
   * Identify similar elements in a given workflow
   * @param wf Workflow
   * @return A set of pair of similar elements
   */
  def similarElements(wf:Workflow) = {
    val similarSet = scala.collection.mutable.Set.empty[WFElement]
    wf.links.foreach(l => wf.links.filterNot(_ == l).map(p => if (p.source == l.source && p.destination ~ l.destination) similarSet.add(l.destination)))
    similarSet.toSet
  }

  /**
   * Identify similar sinks in a given workflow
   * @param wf Workflow
   * @return A set of pair of similar sinks
   */
  def similarSinks(wf:Workflow) = {
    val similarSet = scala.collection.mutable.Set.empty[(WFElement, WFElement)]
    wf.sinks.foreach(e => wf.sinks.filterNot(_ == e).map(p => if ((p ~ e) && (!similarSet.contains((p,e)) && (!similarSet.contains((e,p))))) similarSet.add((p, e))))
    similarSet.toSet
  }

  /**
   * Identify similar sources in a given workflow
   * @param wf Workflow
   * @return A set of pair of similar sources
   */
  def similarSources(wf:Workflow) = {
    val similarSet = scala.collection.mutable.Set.empty[(WFElement, WFElement)]
    wf.sources.foreach(e => wf.sources.filterNot(_ == e).map(p => if ((p ~ e) && (!similarSet.contains((p,e)) && (!similarSet.contains((e,p))))) similarSet.add((p, e))))
    similarSet.toSet
  }

  /**
   * Merge two workflow
   * @param wf1 Workflow left
   * @param wf2 Workflow right
   * @return A merged workflow : wf1+wf2
   */
  def merge(wf1:Workflow, wf2:Workflow):List[Instruction] = {

    val setActions = ArrayBuffer[Instruction]()

    /**
     * @param currentRoot Current left root
     * @param e1 Left element
     * @param e2 Right element
     */
    def merge_internal(currentRoot: WFElement, e1: WFElement, e2: WFElement): Unit = {
      println("Merging: " + e1 + " with " + e2 + " (root: " + currentRoot + ")")
      // Right to left merging

      // Step 1: If e1 ~ e2: do nothing ...
      if (e1 ~ e2) {
        println("\tDEBUG: " + e1 + " ~ " + e2)
        // Step 1a: Get immediate next elements
        val e1Next = wf1.nextElements(e1)
        val e2Next = wf2.nextElements(e2)

        // Step1b: Combine left elements with right elements
        val combination = e1Next.flatMap(e => e2Next.map(f => (e,f)))

        // Step1c : Create pairs of next elements
        val pairs = combination.map(p => ((p._1)._1, (p._2)._1))
        println("\tDEBUG: Next pairs are " + pairs)
        // Step1d : Loop on the pairs with current e1 as root
        pairs.map(p => merge_internal(e1, p._1, p._2))

        // DONE :)

      } else { //step1 : ... else
        println("\tDEBUG: " + e1 + " !~ " + e2)
        //Step 2a: Add e2 into left workflow ...
        setActions += new AddElement(e2)

        if (e2.inputs.headOption.nonEmpty)
          setActions += new AddLink(new WFLink(currentRoot.outputs.head, e2.inputs.head))

        //Step 2b: Get right subworkflow with e2 as root and and it into left
        val subworkflow = wf2.subWorkflow(e2)
        subworkflow.links.foreach(e => (setActions += new AddLink(e)))
        subworkflow.elements.foreach(e => (setActions += new AddElement(e)))
        // DONE :)

      }
    }

    merge_internal(wf1.sources.head, wf1.sources.head, wf2.sources.head)
    println("[DEBUG] End merging: " + setActions)
    setActions.toList

  }

}
