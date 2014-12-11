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
    val newElements = mutable.HashMap[String, WFElement]() // Directory (old --> new)
    val newLinks= mutable.HashMap[String, WFLink]() // Directory (old --> new)

    def convertElement(e: String):WFElement = {
      newElements.get(e) match {
        case Some(a) => println("\t [ALERT] New version found for: " + e + "-->" + a); a // The asked element is outdated and we provide the real element
        case None => throw new NoSuchElementException // The asked element is up to date
      }
    }

    def convertLink(l: String):WFLink = {
      newLinks.get(l) match {
        case Some(a) => println("\t [ALERT] New version found for: (" + l + ") --> (" + a + ")"); a
        case None => throw new NoSuchElementException
      }
    }

    def upToDateElement(e1: WFElement):WFElement = {
      try { convertElement(e1.toString) } catch { case _: NoSuchElementException => e1}
    }

    def upToDateLink(l: WFLink):WFLink = {
      try { convertLink(l.toString)} catch { case _ : NoSuchElementException => l}
    }

    def merge_internal(req1: WFElement, req2: WFElement): Unit = {
      // Update references
      val e1 = upToDateElement(req1)
      val e2 = upToDateElement(req2)

      if (e1 ~ e2) {
        // step 1 : Merge e1 and e2 and delete e1/e2
        val res = (e1 + e2) // We are sure about e1 == e2 property


        println("[Merge] " + e1 + " [WF1] " + e2 + " [WF2] => " + res + " (" + res + ")")
        println("\t* Need to delete [WF1]: " + e1)
        setActions += DeleteElement(e1)
        println("\t* Need to delete [WF2]: " + e2)
        setActions += DeleteElement(e2)
        println("\t* Need to add: " + res)
        setActions += AddElement(res)


        // step 2a : Delete all links with e1 as source and replace by new links with res as source (in both workflow)
        setActions ++= wf1.links.filter(l => upToDateElement(l.source) == e1).foldLeft(List[Instruction]()) { (acc, e) => val created = new WFLink(res.outputs.head, e.destination_input); println("\t* Need to delete link [WF1]: " + e); println("\t* Need to add link [WF1]: " + created); DeleteLink(e) :: AddLink(created) :: acc}
        setActions ++= wf2.links.filter(l => upToDateElement(l.source) == e2).foldLeft(List[Instruction]()) { (acc, e) => val created = new WFLink(res.outputs.head, e.destination_input); println("\t* Need to delete link [WF2]: " + e); println("\t* Need to add link [WF2]: " + created); DeleteLink(e) :: AddLink(created) :: acc}

        // step 2b : Delete all links with e1 as destination and replace by new links with res as source (in both workflow)
        setActions ++= wf1.links.filter(l => upToDateElement(l.destination) == e1).foldLeft(List[Instruction]()) { (acc, e) => val created = new WFLink(e.source_output, res.inputs.head); println("\t* Need to delete link [WF1]: " + e); println("\t* Need to add link [WF1]: " + created); DeleteLink(e) :: AddLink(created) :: acc}
        setActions ++= wf2.links.filter(l => upToDateElement(l.destination) == e2).foldLeft(List[Instruction]()) { (acc, e) => val created = new WFLink(e.source_output, res.inputs.head); println("\t* Need to delete link [WF2]: " + e); println("\t* Need to add link [WF2]: " + created); DeleteLink(e) :: AddLink(created) :: acc}

        newElements += (e1.toString -> res)
        newElements += (e2.toString -> res) // We save the new created element


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
        println("[STOP] Differs from " + e1 + " / " + e2)
      }

    }

    merge_internal(wf1.sources.head, wf2.sources.head)
    setActions.toList

  }







}
