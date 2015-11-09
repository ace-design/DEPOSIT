package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.generator.ArduinoGenerator
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Demo file
 * Created by Cyril Cecchinel - I3S Laboratory on 22/10/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object DemoButtonSinglePlatform extends App{

  /**
   * This demonstration illustrates the counting of pressed button on a single Arduino platform.
   * Presented: 23-10-2015
   */

  // We define three EVENT SENSOR corresponding to each physical button.
  // These sensors produce messages according the SmartCampus data format
  // Values vary between 0 (released position) to 1023 (pressed position)
  // Each sensor has been declared in the assets/sensors/mapping.csv file
  val s1 = EventSensor("BUTTON1", classOf[SmartCampusType])
  val s2 = EventSensor("BUTTON2", classOf[SmartCampusType])
  val s3 = EventSensor("BUTTON3", classOf[SmartCampusType])

  // We add the values produced by the three buttons.
  // ADD operation contains three data inputs upon the SmartCampus format
  // As it is an intermediate step before retrieving the button counting, we do not
  // rename the operation result
  val add = Add(Set("s1", "s2", "s3"), classOf[SmartCampusType])

  // We DIVIDE the ADD result by 1023 to get the number of button pressed.
  // We rename the operation result as "TOTAL_PRESSED_BUTTONS".
  val div = Divide(IntegerType(1023), classOf[SmartCampusType], Some("TOTAL_PRESSED_BUTTONS"))

  // We collect the results
  val c = Collector("output", classOf[SmartCampusType])

  // We define a data flow between the output of BUTTON1 and the first input of ADD operation
  val l1 = Link(s1.output, add.getInput("s1"))
  // We define a data flow between the output of BUTTON2 and the second input of ADD operation
  val l2 = Link(s2.output, add.getInput("s2"))
  // We define a data flow between the output of BUTTON3 and the third input of ADD operation
  val l3 = Link(s3.output, add.getInput("s3"))
  // We define a data flow between the output of ADD operation in the input of DIVIDE operation
  val l4 = Link(add.output, div.input)
  // We define a data flow between the output of DIVIDE operation in the input of the remote collector
  val l5 = Link(div.output, c.input)

  // We build the "BUTTON_DEMO" data collection policy
  val p = new Policy("BUTTON_DEMO")
    .add(s1).add(s2).add(s3).add(add).add(div).add(c)
    .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5)

  // We generate Graphviz and Arduino code
  ToGraphviz.writeSource(p)
  ArduinoGenerator(p, true)
}