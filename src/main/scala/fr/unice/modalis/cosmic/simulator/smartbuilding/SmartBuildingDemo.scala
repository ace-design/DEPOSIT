package fr.unice.modalis.cosmic.simulator.smartbuilding

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.AutoDeploy
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deposit.core.SmartCampusType
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */

object OfficePolicy extends DEPOSIT {
  this hasForName "ALERT_AC"
  this handles classOf[SmartCampusType]

  val ac_sensor = declare aGenericInput() named "AC"
  val door_sensor = declare aGenericInput() named "DOOR"
  val window_sensor = declare aGenericInput() named "WINDOW"

  val door_converter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val window_converter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val ac_threshold = define aFilter "v < 19"

  val aggregator = define aProducer new SmartCampusType("ALERT_AC", 1)  withInputs("i1", "i2", "i3")
  val result = declare aGenericOutput() named "RESULT"

  flows {
    ac_sensor() -> ac_threshold("input")
    door_sensor() -> door_converter("input")
    window_sensor() -> window_converter("input")

    ac_threshold("then") -> aggregator("i1")
    door_converter("open") -> aggregator("i2")
    window_converter("open") -> aggregator("i3")

    aggregator("output") -> result()
  }
}

object SmartBuildingDemo extends DEPOSIT{

  this hasForName "Implem_ALERT_AC_BULDING"
  this handles classOf[SmartCampusType]
  this targets "assets/configurations/demo_smartbuilding.xml"

  for (i <- 401 to 450) {
    val door = declare anEventSensor() named s"DOOR_$i"
    val window = declare anEventSensor() named s"WINDOW_$i"
    val ac = declare aPeriodicSensor() named s"AC_$i" withPeriod 300

    val process = define aProcess OfficePolicy()

    val collector = declare aCollector() named "SmartCampus"

    flows {
      door() -> process("DOOR")
      window() -> process("WINDOW")
      ac() -> process("AC")
      process("RESULT") -> collector()
    }
  }





}

object MyApp extends App{

  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")
  val infraModel = InfrastructureModel(topology, DeploymentRepartition.CLOSEST_TO_SENSORS)

  val repo = Repository(infraModel)
  RepositoriesManager.addRepository(repo)

  //SmartBuildingDemo().exportToGraphviz()

  val t1 = System.currentTimeMillis
  AutoDeploy(SmartBuildingDemo(), infraModel)
  val t2 = System.currentTimeMillis
  println((t2 - t1) + " msecs")


}

