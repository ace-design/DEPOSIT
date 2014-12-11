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


  def merge(wf1:Workflow, wf2:Workflow):List[Instruction] = {

    val setActions = ArrayBuffer[Instruction]()


    def merge_internal(e1: WFElement, e2: WFElement): Unit = {
      // Right to left merging

      // Step 1: If e1 ~ e2: do nothing else add e2 into left workflow
      if (e1 ~ e2) {
        println(e1 + " ~ " + e2)

        // Step 2: Get e1 and e2 next elements
        val e1Next = wf1.nextElements(e1)
        val e2Next = wf2.nextElements(e2)

        // Step 3: Combine e1Next with e2Next
        val combination = e1Next.flatMap(e => e2Next.map(f => (e,f)))

        // Step 4: If two elements are not similar:  ...
        combination.foreach(p => if (!((p._1)._1 ~ (p._2)._1)){
          println((p._1)._1 + " !~ " + (p._2)._1)
          // ... add element into left workflow ...
          setActions += new AddElement((p._2)._1)
          // ... add a link between root element and this new element ...
          val output = ((p._1)._2).source_output
          val input = ((p._2)._2).destination_input
          setActions += new AddLink(new WFLink(output, input))
          // ... Copy right sub-workflow with (p._2)._1 as root
          val sub = wf2.subWorkflow((p._2)._1)
          sub.elements.foreach(setActions += new AddElement(_))
          sub.links.foreach(setActions += new AddLink(_))

        } else {
          println((p._1)._1 + " ~ " + (p._2)._1)
          // Step 4bis : If two elements are similar ...
          // ... loop on e1/e2 next elements
          val nextElementWF1 = wf1.nextElements(e1).map(_._1)
          val nextElementWF2 = wf2.nextElements(e2).map(_._1)
          (nextElementWF1.toList, nextElementWF2.toList) match {
            case (a :: _, b :: _) => merge_internal(a, b) // Loop
            case (_, _) => // Finished
          }
        }
        )
      } else {
        println(e1 + " !~ " + e2)

        // Step 1 bis
        val sub = wf2.subWorkflow(e2)
        sub.elements.foreach(setActions += new AddElement(_))
        sub.links.foreach(setActions += new AddLink(_))
      }
    }

    merge_internal(wf1.sources.head, wf2.sources.head)
    setActions.toList

  }







}
