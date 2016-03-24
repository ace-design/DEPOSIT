package fr.unice.modalis.cosmic.simulator.smartbuilding

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.generator.CodeGenerator
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */
protected object ExperimentalValues {
  val RANGE = 401 to 450
  val STRATEGY = DeploymentRepartition.CLOSEST_TO_SENSORS
  val PERIOD = 300
}
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

  for (i <- ExperimentalValues.RANGE) {
    val door = declare anEventSensor() named s"DOOR_$i"
    val window = declare anEventSensor() named s"WINDOW_$i"
    val ac = declare aPeriodicSensor() named s"AC_$i" withPeriod ExperimentalValues.PERIOD

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

protected object GlobalCelsiusToFahrenheitConverter extends DEPOSIT {
  this hasForName "CelsiusToFahrenheit"
  this handles classOf[SmartCampusType]
  this targets "assets/configurations/demo_smartbuilding.xml"

  for (i <- ExperimentalValues.RANGE) {
    val temp = declare aPeriodicSensor() named s"TEMP_$i" withPeriod ExperimentalValues.PERIOD
    val process = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
    val collector = declare aCollector() named "SmartCampus"

    flows {
      temp() -> process("celsius")
      process("fahrenheit") -> collector()
    }
  }
}
protected object OfficeConverterBuilder extends DEPOSIT {
  def apply(office:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val temp = PeriodicSensor(ExperimentalValues.PERIOD, s"AC_$office", dataType)
    val process = Process(StandardizedPolicies.CelsiusToFahrenheit(), dataType, dataType)
    val collector = Collector("SmartCampus", dataType)

    val l1 = Flow(temp.output, process.getInput("celsius"))
    val l4 = Flow(process.getOutput("fahrenheit"), collector.input)

    new Policy(s"Implem_TEMP_$office").add(temp).add(process).add(collector).add(l1).add(l4)

  }
}
protected object OfficeBuilder extends DEPOSIT {
  def apply(office:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val door = EventSensor(s"DOOR_$office", dataType)
    val window = EventSensor(s"WINDOW_$office", dataType)
    val ac = PeriodicSensor(ExperimentalValues.PERIOD, s"AC_$office", dataType)
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
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)

  val t1 = System.currentTimeMillis()
  Deploy(PreDeploy(OfficeBuilder("401"), topology), topology, ExperimentalValues.STRATEGY).foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }
  val t2 = System.currentTimeMillis()

  println(s"Time elapsed to deploy one policy: ${t2 - t1} ms")
}

object GlobalPolicy extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")

  val t1 = System.currentTimeMillis()
  GlobalSmartBuildingDemo.deploy().foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }
  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a global policy: ${t2 - t1} ms")
}

object GlobalPolicyWithComposition extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)
  val t1 = System.currentTimeMillis()
  val p = ExperimentalValues.RANGE.foldLeft(new Policy("")){(acc, e) => acc ++ OfficeBuilder(e.toString)}
  val preDeploy = PreDeploy(p, topology)
  Deploy(preDeploy, topology, ExperimentalValues.STRATEGY).foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }
  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a composed policy: ${t2 - t1} ms")
}

object GlobalPolicyConverter extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")

  val t1 = System.currentTimeMillis()
  val p = ExperimentalValues.RANGE.foldLeft(new Policy("")){(acc, e) => acc ++ OfficeConverterBuilder(e.toString)}

  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  deploy.foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }

  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a global policy: ${t2 - t1} ms")
}

object TwoPoliciesWithComposition extends App {
  val topology = TopologyModelBuilder("assets/configurations/demo_smartbuilding.xml")




 val policy1 = ExperimentalValues.RANGE.foldLeft(new Policy("")){(acc, e) => acc ++ OfficeBuilder(e.toString)}
 val policy2 = ExperimentalValues.RANGE.foldLeft(new Policy("")){(acc, e) => acc ++ OfficeConverterBuilder(e.toString)}

 val composition = policy1 ++ policy2

  val t1 = System.currentTimeMillis()

  println(s"Concepts before expend ${composition.concepts.size}")
  val preDeploy = PreDeploy(composition, topology)
  println(preDeploy.ios)

  println(s"Concepts after expend ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)
  deploy.foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }

  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy the two policies: ${t2 - t1} ms")

}