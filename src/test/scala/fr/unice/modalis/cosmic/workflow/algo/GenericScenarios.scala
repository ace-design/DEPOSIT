package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
object GenericScenarios {

  // Implementation scenario 2 : Fire prevention: a fire alert if a temperature threshold is reached
  val validWF:Workflow[IntegerType] = {

    val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

    val periodicGetter = new PeriodicGetter[IntegerType](30000)

    val predicate = new Predicate[IntegerType](ValueConstraint(">", 50))


    val collector = new Sink[IntegerType]("alice")

    new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
      .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)).addLink(new WFLink[IntegerType](periodicGetter.output, predicate.input))
      .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input))

  }

  val invalidWF:Workflow[IntegerType] = {

    val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

    val predicate = new Predicate[IntegerType](ValueConstraint(">", 50))

    val collector = new Sink[IntegerType]("alice")

    new Workflow[IntegerType]().addElement(temperatureSensor).addElement(predicate).addElement(collector)
      .addLink(new WFLink[IntegerType](temperatureSensor.output, predicate.input))
      .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input))

  }

  //END Scenario 2

  val validWFWithLoop:Workflow[IntegerType] = {

    val temperatureSensor = new Source[IntegerType]("TEMP_SENSOR")

    val periodicGetter = new PeriodicGetter[IntegerType](30000)

    val predicate = new Predicate[IntegerType](ValueConstraint(">", 50))


    val collector = new Sink[IntegerType]("alice")

    new Workflow[IntegerType]().addElement(temperatureSensor).addElement(periodicGetter).addElement(predicate).addElement(collector)
      .addLink(new WFLink[IntegerType](temperatureSensor.output, periodicGetter.input)).addLink(new WFLink[IntegerType](periodicGetter.output, predicate.input))
      .addLink(new WFLink[IntegerType](predicate.trueOutput,collector.input)).addLink(new WFLink[IntegerType](predicate.falseOutput, predicate.input))

  }
}
