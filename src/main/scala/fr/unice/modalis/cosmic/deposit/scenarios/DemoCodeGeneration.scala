package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.generator.ArduinoGenerator
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 24/09/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object DemoCodeGeneration extends App{

  val s1 = EventSensor("BUTTON1", classOf[SmartCampusType])
  val s2 = EventSensor("BUTTON2", classOf[SmartCampusType])
  val s3 = EventSensor("BUTTON3", classOf[SmartCampusType])
  val add = Add(Set("s1", "s2", "s3"), classOf[SmartCampusType])
  val div = Divide(IntegerType(1023), classOf[SmartCampusType], Some("TOTAL_OCCUPIED_SPACES"))
  val c = Collector("output", classOf[SmartCampusType])
  val l1 = Link(s1.output, add.getInput("s1"))
  val l2 = Link(s2.output, add.getInput("s2"))
  val l3 = Link(s3.output, add.getInput("s3"))
  val l4 = Link(add.output, div.input)
  val l5 = Link(div.output, c.input)
  val p = new Policy("demo").add(s1).add(s2).add(s3).add(add).add(div).add(c).addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5)

  ArduinoGenerator(p, true)
}
