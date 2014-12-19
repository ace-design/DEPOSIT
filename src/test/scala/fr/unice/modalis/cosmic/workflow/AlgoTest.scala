package fr.unice.modalis.cosmic.workflow

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.algo.Algo
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/12/14.
 */
class AlgoTest extends SpecificationWithJUnit{

  "The similar algorithm identify the same level similar elements in a workflow" in {
    val sourceSensor = new Source[IntegerType]("TEMP442")
    val filter_value1 = new IntegerFilter(new ValueConstraint("<", new IntegerType(15)))
    val filter_value2 = new IntegerFilter(new ValueConstraint("<", new IntegerType(15)))
    val sink1 = new Sink[IntegerType]("smartcampus")
    val sink2 = new Sink[IntegerType]("smartcampus")

    val l1 = new WFLink(sourceSensor.output, filter_value1.input)
    val l2 = new WFLink(sourceSensor.output, filter_value2.input)
    val l3 = new WFLink(filter_value1.output, sink1.input)
    val l4 = new WFLink(filter_value2.output, sink2.input)

    val wf = new Workflow(Set(sourceSensor, filter_value1, filter_value2, sink1, sink2), Set(l1, l2, l3, l4))
    Algo.similar(wf) must_== Set(filter_value1, filter_value2)
  }


}
