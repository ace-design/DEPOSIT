package fr.unice.modalis.cosmic.workflow

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
class WorkflowTest extends SpecificationWithJUnit{

  "An element should be added in a given workflow" in {
    val toAdd = new Sink[DoubleType]("test")

    val wf = new Workflow

    wf.addElement(toAdd).elements.contains(toAdd) must beTrue
  }

  "A link should be added in a given workflow" in {
    val source = new Source[DoubleType]("TEMP")
    val sink = new Sink[DoubleType]("alice")

    val toAdd = new WFLink(source.output, sink.input)

    val wf = new Workflow().addElement(source).addElement(sink)

    wf.addLink(toAdd).links.contains(toAdd) must beTrue
  }

  "Elements must be unique in a given workflow" in {
    val source = new Source[DoubleType]("TEMP")
    val wf = new Workflow().addElement(source).addElement(source)

    wf.elements.size must_== 1
  }

  /* DELETION TESTS */
  val sourceSensor = new Source[IntegerType]("TEMP")
  val clock = new Source[LongType]("CLK")

  val filter_clock = new IntegerLongFilter(null)

  val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(50)))

  val sink = new Sink[IntegerType]("alice")

  val l1 = new WFLink(sourceSensor.output, filter_clock.integerInput)
  val l2 = new WFLink(clock.output, filter_clock.longInput)
  val l3 = new WFLink(filter_clock.output, filter_value.input)
  val l4 = new WFLink(filter_value.output, sink.input)

  val wf = new Workflow().addElement(sourceSensor).addElement(clock).addElement(filter_clock).addElement(filter_value)
    .addLink(l1).addLink(l2).addLink(l3).addLink(l4)

  "The deletion of an element must delete links refering this elements" in {
    wf.deleteElement(filter_value).links.filter(p => (p.source == filter_value) || (p.destination == filter_value)).size must_== 0

  }

  "The deletion of an element must remove it from the filter_value" in {
    wf.deleteElement(filter_value).elements.contains(filter_value) must beFalse
  }

  /* PATH TESTS */
  "Next element test" in {
    wf.nextElements(sourceSensor) mustEqual Set((filter_clock, l1))
  }
}
