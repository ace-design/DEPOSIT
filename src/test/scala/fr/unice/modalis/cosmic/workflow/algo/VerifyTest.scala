package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
class VerifyTest extends SpecificationWithJUnit{

  // Implementation scenario 2 : Fire prevention: a fire alert if a temperature threshold is reached
  val validWF:Workflow[IntegerType] = {

    val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

    val periodicGetter = new PeriodicGetter[IntegerType](30000)

    val predicate = new Predicate[IntegerType](p => p.value > 50)

    val collector = new Sink[IntegerType]("alice")

    new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
    .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)).addLink(new WFLink[IntegerType](periodicGetter.output, predicate.input))
    .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input))

  }

  val invalidWF:Workflow[IntegerType] = {

    val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

    val predicate = new Predicate[IntegerType](p => p.value > 50)

    val collector = new Sink[IntegerType]("alice")

    new Workflow[IntegerType]().addElement(temperatureSensor).addElement(predicate).addElement(collector)
      .addLink(new WFLink[IntegerType](temperatureSensor.output, predicate.input))
      .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input))

  }

  //END Scenario 2

  //Links
  val link1 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new Predicate[IntegerType](p => p.value > 0).input) //Invalid

  val link2 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new PeriodicGetter[IntegerType](10000).input) //Valid

  "A valid link must pass checkLink" in {
    Verify.checkLink(link2) must beTrue
  }

  "A invalid link must failed" in {
    Verify.checkLink(link1) must beFalse
  }

}
