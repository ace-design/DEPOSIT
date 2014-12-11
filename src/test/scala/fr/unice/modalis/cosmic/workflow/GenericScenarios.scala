package fr.unice.modalis.cosmic.workflow

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
object GenericScenarios {

  val easy = {
    val sourceSensor = new Source[IntegerType]("TEMP")

    val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(50)))

    val sink = new Sink[IntegerType]("alice")

    val l1 = new WFLink(sourceSensor.output, filter_value.input)
    val l2 = new WFLink(filter_value.output, sink.input)

    new Workflow().addElement(sourceSensor).addElement(filter_value).addElement(sink).addLink(l1).addLink(l2)

  }

  val twoSources = {
    val sourceSensor = new Source[IntegerType]("TEMP")
    val clock = new Source[IntegerType]("CLK")
    val filter_clock = new IntegerLongFilter(new ValueConstraint("==", new IntegerType(1)))
    val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(24)))
    val sink = new Sink[IntegerType]("smartcampus")


    val l1 = new WFLink(sourceSensor.output, filter_clock.integerInput)
    val l2 = new WFLink(clock.output, filter_clock.longInput)
    val l3 = new WFLink(filter_clock.output, filter_value.input)
    val l4 = new WFLink(filter_value.output, sink.input)


    new Workflow(Set(sourceSensor, clock, filter_clock, filter_value, sink), Set(l1,l2,l3,l4))
  }


  val error1 = { // Missing link
    val sourceSensor = new Source[IntegerType]("TEMP")
    val clock = new Source[IntegerType]("CLK")
    val filter_clock = new IntegerLongFilter(new ValueConstraint("==", new IntegerType(1)))
    val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(24)))
    val sink = new Sink[IntegerType]("smartcampus")


    val l1 = new WFLink(sourceSensor.output, filter_clock.integerInput)
    val l2 = new WFLink(clock.output, filter_clock.longInput)
    val l4 = new WFLink(filter_value.output, sink.input)


    new Workflow(Set(sourceSensor, clock, filter_clock, filter_value, sink), Set(l1,l2,l4))
  }

  val noSink = {
    val sourceSensor = new Source[IntegerType]("TEMP")
    val clock = new Source[IntegerType]("CLK")
    val filter_clock = new IntegerLongFilter(new ValueConstraint("==", new IntegerType(1)))
    val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(24)))


    val l1 = new WFLink(sourceSensor.output, filter_clock.integerInput)
    val l2 = new WFLink(clock.output, filter_clock.longInput)
    val l3 = new WFLink(filter_clock.output, filter_value.input)


    new Workflow(Set(sourceSensor, clock, filter_clock, filter_value), Set(l1,l2,l3))
  }




}