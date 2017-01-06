package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 06/10/2016.
  */
object DemoLearningCentre extends App with DEPOSIT{

  this hasForName "DemoLearningCentre"
  this handles classOf[SmartCampusType]
  this targets "assets/configurations/LearningCentreSophia.xml"

  val soundSensor = declare aPeriodicSensor() named "SOUND_1" withPeriod 1

  val filter1 = define aFilter "value < 300"
  val filter2 = define aFilter "value < 600"

  val rename1 = define aRenamerTo "LOW_LC_1"
  val rename2 = define aRenamerTo "MEDIUM_LC_1"
  val rename3 = define aRenamerTo "HIGH_LC_1"

  val collector = declare aCollector() named "SmartCampus"
  val collector2 = declare aCollector() named "SmartCampus"
  val collector3 = declare aCollector() named "SmartCampus"

  flows {
    soundSensor() -> filter1("input")
    filter1("then") -> rename1("input")
    filter1("else") -> filter2("input")
    filter2("then") -> rename2("input")
    filter2("else") -> rename3("input")

    rename1("output") -> collector()
    rename2("output") -> collector2()
    rename3("output") -> collector3()

  }

  val p = deploy()

  p.head.exportToGraphviz()
  p.head.exportToWiring()

}