package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 23/11/2015.
  */
object DemoAlertACWithDSL extends App with DEPOSIT{

  this hasForName "DemoDSL"

  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 3 handling classOf[SmartCampusType]
  val door_443 = declare anEventSensor() named "DOOR_443" handling  classOf[SmartCampusType]
  val window_443 = declare anEventSensor() named "WINDOW_443" handling classOf[SmartCampusType]



  val temp_filter = define aFilter "value < 18" handling classOf[SmartCampusType]
  val door_filter = define aFilter "value < 500" handling classOf[SmartCampusType]
  val window_filter = define aFilter "value < 500" handling classOf[SmartCampusType]

  val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3") handingOnInputs classOf[SmartCampusType] andHandlingOnOutputs classOf[SmartCampusType]

  val collector = declare aCollector() named "Collector" handling classOf[SmartCampusType]

  flows {
    ac_443() -> temp_filter("input")
    door_443() -> door_filter("input")
    window_443() -> window_filter("input")
    temp_filter("then") -> produce("i1")
    window_filter("then") -> produce("i2")
    door_filter("then") -> produce("i3")
    produce("output") -> collector()
  }

  this targets "assets/configurations/smartcampus_xbeenetwork.xml"
  val policies = deploy()

  policies.foreach(_.exportToGraphviz)
}
