package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.generator.ArduinoGenerator
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Demo file
 * Created by Cyril Cecchinel - I3S Laboratory on 23/10/15.
 */
object DemoAlertAC extends App{

  /**
   * This demonstration illustrates how three sensors can infer a new information
   * Scenario: Send an alert (ALERT_AC) if window (WINDOW_443) and door (DOOR_443) are opened
   *           while air conditioning is powered on
   * Presented: 29-10-2015
   */

  /**
   * We define the sensors deployed in the monitored office
   * Each sensor has been declared in the assets/sensors/mapping.csv file
   */

  // Air conditioning temperature sensor (Period = 60s, value in celsius degrees)
  val ac443 = PeriodicSensor(60, "AC_443", classOf[SmartCampusType]) //Pin 0

  // Door and window opening sensors (Event-based, value : < 500 Open, >= 500 Closed)
  val door443 = EventSensor("DOOR_443", classOf[SmartCampusType]) //Pin 1
  val window443 = EventSensor("WINDOW_443", classOf[SmartCampusType]) //Pin 2

  // We use filter operations to check if air conditioning is on (temperature < 18°C)
  // and door/window opened (value < 500 => Open)
  val temp_filter = Conditional("value < 18", classOf[SmartCampusType])
  val door_filter = Conditional("value < 500", classOf[SmartCampusType])
  val window_filter = Conditional("value < 500", classOf[SmartCampusType])

  // When all filter produce data (ie. air conditioning is on and Window/Door opened), produce
  // a data upon the SmartCampus format named ALERT_AC with value 1.
  val produce = Produce(Set("i1", "i2", "i3"), new SmartCampusType("ALERT_AC", 1), None, classOf[SmartCampusType], classOf[SmartCampusType])

  // We collect the results
  val collector = Collector("collector", classOf[SmartCampusType])

  /**
   * We define data flows between the concepts
   */
  val l1 = Link(ac443.output, temp_filter.input)
  val l2 = Link(door443.output, door_filter.input)
  val l3 = Link(window443.output, window_filter.input)
  val l4 = Link(temp_filter.thenOutput, produce.getInput("i1"))
  val l5 = Link(door_filter.thenOutput, produce.getInput("i2"))
  val l6 = Link(window_filter.thenOutput, produce.getInput("i3"))
  val l7 = Link(produce.output, collector.input)

  // We build the "ALERT_AC" data collection policy
  val p = new Policy("ALERT_AC").add(window443).add(ac443).add(collector).add(door443)
    .add(temp_filter).add(door_filter).add(window_filter).add(produce)
    .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5).addLink(l6).addLink(l7)

  // We generate Arduino code
  ArduinoGenerator(p, true)
}
