package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 23/11/2015.
  */
object DemoAlertACWithDSL extends DEPOSIT{

  this hasForName "DemoDSL"
  this handles classOf[SmartCampusType]


  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 3
  val door_443 = declare anEventSensor() named "DOOR_443"
  val window_443 = declare anEventSensor() named "WINDOW_443"



  val temp_filter = define aFilter "value < 18"
  val door_filter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val window_filter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()

  val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3")

  val collector = declare aCollector() named "SmartCampus"

  flows {
    ac_443() -> temp_filter("input")
    door_443() -> door_filter("input")
    window_443() -> window_filter("input")
    temp_filter("then") -> produce("i1")
    window_filter("open") -> produce("i2")
    door_filter("open") -> produce("i3")
    produce("output") -> collector()
  }


}

