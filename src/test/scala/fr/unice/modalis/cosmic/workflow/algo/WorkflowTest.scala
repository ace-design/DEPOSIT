package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
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

  "Elements must be unique in a given workflow" in {
    val source = new Source[DoubleType]("TEMP")
    val wf = new Workflow[DoubleType].addElement(source).addElement(source)

    wf.elements.size must_== 1
  }

  /* DELETION TESTS */
  val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

  val periodicGetter = new PeriodicGetter[IntegerType](30000)

  val predicate = new Predicate[IntegerType](ValueConstraint(">", 50))


  val collector = new Sink[IntegerType]("alice")

  val l1 = new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)
  val l2 = new WFLink[IntegerType](periodicGetter.output, predicate.input)
  val l3 = new WFLink[IntegerType](predicate.trueOutput,collector.input)

  val wf = new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
    .addLink(l1).addLink(l2).addLink(l3)

  "The deletion of an element must delete links refering this elements" in {
    wf.deleteElement(predicate).links.filter(p => (p.source == predicate) || (p.destination == predicate)).size must_== 0

  }

  "The deletion of an element must remove it from the workflow" in {
    wf.deleteElement(predicate).elements.contains(predicate) must beFalse
  }

  /* PATH TESTS */
  "Next element test" in {
    wf.nextElements(predicate) mustEqual Set((collector, l3))
  }
}
