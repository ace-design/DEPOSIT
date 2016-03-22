package fr.unice.modalis.cosmic.simulator.smartbuilding

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */

protected object OfficePolicy extends DEPOSIT {
  this hasForName "ALERT_AC"
  this handles classOf[SmartCampusType]

  val ac_sensor = declare aGenericInput() named "AC"
  val door_sensor = declare aGenericInput() named "DOOR"
  val window_sensor = declare aGenericInput() named "WINDOW"

  val door_converter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val window_converter = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val ac_threshold = define aFilter "value < 19"

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

protected object GlobalSmartBuildingDemo extends DEPOSIT{

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


protected object OfficeBuilder extends DEPOSIT {
  def apply(office:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val door = EventSensor(s"DOOR_$office", dataType)
    val window = EventSensor(s"WINDOW_$office", dataType)
    val ac = PeriodicSensor(300, s"AC_$office", dataType)
    val process = Process(OfficePolicy(), dataType, dataType)
    val collector = Collector("SmartCampus", dataType)

    val l1 = Flow(door.output, process.getInput("DOOR"))
    val l2 = Flow(window.output, process.getInput("WINDOW"))
    val l3 = Flow(ac.output, process.getInput("AC"))
    val l4 = Flow(process.getOutput("RESULT"), collector.input)

    new Policy(s"Implem_ALERT_AC_$office").add(door).add(window).add(ac).add(process).add(collector).add(l1).add(l2).add(l3).add(l4)

  }
}

object OnePolicy extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")
  val infraModel = InfrastructureModel(topology, DeploymentRepartition.CLOSEST_TO_SENSORS)

  val t1 = System.currentTimeMillis()
  Deploy(PreDeploy(OfficeBuilder("401"), topology), topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
  val t2 = System.currentTimeMillis()

  println(s"Time elapsed to deploy one policy: ${t2 - t1} ms")
}

object GlobalPolicy extends App with DEPOSIT{
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")

  val t1 = System.currentTimeMillis()
  GlobalSmartBuildingDemo.deploy()
  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a global policy: ${t2 - t1} ms")
}

object GlobalPolicyWithComposition extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")
  val infraModel = InfrastructureModel(topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
  val t1 = System.currentTimeMillis()
  val p = (401 to 450).foldLeft(new Policy("")){(acc, e) => acc ++ OfficeBuilder(e.toString)}
  val preDeploy = PreDeploy(p, topology)
  Deploy(preDeploy, topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a composed policy: ${t2 - t1} ms")
}
