package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
class VerifyTest extends SpecificationWithJUnit{

  val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

  val periodicGetter = new PeriodicGetter[IntegerType](30000)

  val predicate = new Predicate[IntegerType](ValueConstraint(">", 50))

  val predicate2 = new Predicate[IntegerType](ValueConstraint(">", 20))

  val collector = new Sink[IntegerType]("alice")

  // Correct
  val wf = new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
    .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)).addLink(new WFLink[IntegerType](periodicGetter.output, predicate.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input)).addLink(new WFLink[IntegerType](predicate.falseOutput, predicate.input))


  // Sink is not collector
  val wf2 = new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector).addElement(predicate2)
    .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)).addLink(new WFLink[IntegerType](periodicGetter.output, predicate.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input)).addLink(new WFLink[IntegerType](predicate.falseOutput, predicate.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput, predicate2.input))

  // No connectivity
  val wf3 = new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
    .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input)).addLink(new WFLink[IntegerType](predicate.falseOutput, predicate.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput, predicate2.input))


  //Links
  val link1 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new Predicate[IntegerType](ValueConstraint("==", 2)).input) //Invalid

  val link2 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new PeriodicGetter[IntegerType](10000).input) //Valid

  "A valid link must pass checkLink" in {
    Verify.checkLink(link2) must beTrue
  }

  "A invalid link must failed" in {
    Verify.checkLink(link1) must beFalse
  }

  "A user workflow has to be connected" in {
    Verify.connectivity(wf) must beTrue
  }

  "A user workflow has to be connected (2)" in {
    Verify.connectivity(wf3) must beFalse
  }


  "A workflow must have sinks" in {
    Verify.collectorSink(wf) must beTrue
  }

  "A workflow must have sinks (2)" in {
    Verify.collectorSink(wf2) must beFalse
  }
}
