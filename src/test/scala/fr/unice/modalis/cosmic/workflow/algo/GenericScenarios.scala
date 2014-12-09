package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
object GenericScenarios {

  val easy = {
    val sourceSensor = new Source[IntegerType]("TEMP")
    val clock = new Source[LongType]("CLK")

    val filter_clock = new IntegerLongFilter(null)

    val filter_value = new IntegerFilter(new ValueConstraint(">", new IntegerType(50)))

    val sink = new Sink[IntegerType]("alice")

    val l1 = new WFLink(sourceSensor.output, filter_clock.integerInput)
    val l2 = new WFLink(clock.output, filter_clock.longInput)
    val l3 = new WFLink(filter_clock.output, filter_value.input)
    val l4 = new WFLink(filter_value.output, sink.input)

    new Workflow().addElement(sourceSensor).addElement(clock).addElement(filter_clock).addElement(filter_value)
      .addLink(l1).addLink(l2).addLink(l3).addLink(l4)

  }


}
