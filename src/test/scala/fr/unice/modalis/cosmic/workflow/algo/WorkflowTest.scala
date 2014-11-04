package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
class WorkflowTest extends SpecificationWithJUnit{

  "An element should be added in a given workflow" in {
    val toAdd = new Sink[DoubleType]("test")

    val wf = new Workflow[DoubleType]

    wf.addElement(toAdd).elements.contains(toAdd) must beTrue
  }

  "A link should be added in a given workflow" in {
    val source = new Source[DoubleType]("TEMP")
    val sink = new Sink[DoubleType]("alice")

    val toAdd = new WFLink[DoubleType](source.output, sink.input)

    val wf = new Workflow[DoubleType].addElement(source).addElement(sink)

    wf.addLink(toAdd).links.contains(toAdd) must beTrue
  }
}
