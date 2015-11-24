package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
 * Demo file
 * Created by Cyril Cecchinel - I3S Laboratory on 22/10/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object DemoButtonSinglePlatformWithDSL extends App with DEPOSIT{

  /**
   * This demonstration illustrates the counting of pressed button on a single Arduino platform.
   * It uses the DEPOSIT language
   */
  this hasForName "DemoButtonSinglePlatformWithDSL"

  // We define three EVENT SENSOR corresponding to each physical button.
  // These sensors produce messages according the SmartCampus data format
  // Values vary between 0 (released position) to 1023 (pressed position)
  // Each sensor has been declared in the assets/sensors/mapping.csv file

  val s1 = declare anEventSensor() named "BUTTON1" handling classOf[SmartCampusType]
  val s2 = declare anEventSensor() named "BUTTON2" handling classOf[SmartCampusType]
  val s3 = declare anEventSensor() named "BUTTON3" handling classOf[SmartCampusType]

  // We add the values produced by the three buttons.
  // ADD operation contains three data inputs upon the SmartCampus format
  // As it is an intermediate step before retrieving the button counting, we do not
  // rename the operation result
  val add = define anAdder() withInputs("i1", "i2", "i3") handling classOf[SmartCampusType]

  // We DIVIDE the ADD result by 1023 to get the number of button pressed.
  // We rename the operation result as "TOTAL_PRESSED_BUTTONS".
  val div = define aDividerBy IntegerType(1023) handling classOf[SmartCampusType] andRenameData "TOTAL_PRESSED_BUTTON"

  // We collect the results
  val c = declare aCollector() named "output" handling classOf[SmartCampusType]


  // We define data flows
  flows {
    s1() -> add("i1")
    s2() -> add("i2")
    s3() -> add("i3")
    add("output") -> div("input")
    div("output") -> c()
  }

  // We generate Graphviz and Arduino code
  exportToGraphviz()
  exportToWiring()
}
