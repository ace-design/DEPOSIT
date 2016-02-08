package fr.unice.modalis.cosmic

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * This is a specification test of a data collection policy
  * defined with the DEPOSIT DSL
  * Created by Cyril Cecchinel - I3S Laboratory on 20/01/2016.
  */

object ComprehensivePolicy extends DEPOSIT {

  this hasForName "DemoPolicy"
  this handles classOf[SmartCampusType]


  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 300
  val window_sensor = declare anEventSensor() named "WINDOW_443"
  val door_sensor = declare anEventSensor() named "DOOR_443"

  val celsiusToFahrenheit = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
  val openingConverterForWindow = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val openingConverterForDoor = define aProcess StandardizedPolicies.RawValueToOpeningSensor()

  val threshold = define aFilter "v < 64"
  val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3")

  val collector = declare aCollector() named "Collector"

  flows {
    ac_443() -> celsiusToFahrenheit("celsius")
    celsiusToFahrenheit("fahrenheit") -> threshold("input")
    threshold("then") -> produce("i3")

    window_sensor() -> openingConverterForWindow("input")
    openingConverterForWindow("open") -> produce("i2")

    door_sensor() -> openingConverterForDoor("input")
    openingConverterForDoor("open") -> produce("i1")

    produce("output") -> collector()

  }

  // Allow the retrieval of the inner data collection policy
  def innerPolicy() = policy

}

