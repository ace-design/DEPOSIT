package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.algo.vm.{AddLink, AddElement}
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
class VMTest extends SpecificationWithJUnit{


  val newCollector = new Sink[IntegerType]("falseResults")

  "a AddElement action should add an element to a given workflow" in {
    val action = new AddElement(newCollector)

    action.make(GenericScenarios.validWF).elements.contains(newCollector) must beTrue
  }


  "a AddLink action should add an element to a given workflow" in {
    val source = new Source[IntegerType]("TEMP")
    val sink = new Sink[IntegerType]("bob")
    val wf = new Workflow[IntegerType]().addElement(source).addElement(sink)

    val newLink = new WFLink[IntegerType](source.output, sink.input)
    val action = new AddLink(newLink)
    action.make(wf).links.contains(newLink) must beTrue
  }
}
