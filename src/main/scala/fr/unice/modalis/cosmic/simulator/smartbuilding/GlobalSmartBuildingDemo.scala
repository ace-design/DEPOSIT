package fr.unice.modalis.cosmic.simulator.smartbuilding

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.generator.CodeGenerator
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import fr.unice.modalis.cosmic.simulator.ExperimentalValues



object ParkingSpaceMonitoringBuilder extends DEPOSIT {
  def apply(place:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val sensor = EventSensor(s"PRK_$place", dataType)
    val filter = Conditional("value == 0", dataType)
    val produceFree = Produce(Set("i1"), new SmartCampusType(place, 1), None, dataType, dataType)
    val produceOccupied = Produce(Set("i1"), new SmartCampusType(place, 0), None, dataType, dataType)

    val collector = Collector("SERVER_1", dataType)
    val collector2 = collector.duplicate

    val l1 = Flow(sensor.output, filter.input)
    val l2 = Flow(filter.thenOutput, produceFree.getInput("i1"))
    val l3 = Flow(produceFree.output, collector.input)
    val l4 = Flow(filter.elseOutput, produceOccupied.getInput("i1"))
    val l5 = Flow(produceOccupied.output, collector2.input)

    new Policy(s"Implem_PRK_Monitoring_$place").add(sensor).add(filter).add(produceFree).add(produceOccupied).add(collector)
    .add(collector2).add(l1).add(l2).add(l3).add(l4).add(l5)

  }
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
  this targets ExperimentalValues.INFRA_XML

  for (i <- ExperimentalValues.RANGE_OFFICE) {
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
  this targets ExperimentalValues.INFRA_XML

  for (i <- ExperimentalValues.RANGE_OFFICE) {
    val temp = declare aPeriodicSensor() named s"TEMP_$i" withPeriod ExperimentalValues.PERIOD
    val process = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
    val collector = declare aCollector() named "SmartCampus"

    flows {
      temp() -> process("celsius")
      process("fahrenheit") -> collector()
    }
  }
}
object OfficeConverterBuilder extends DEPOSIT {
  def apply(office:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val temp = PeriodicSensor(ExperimentalValues.PERIOD, s"AC_$office", dataType)
    val process = Process(StandardizedPolicies.CelsiusToFahrenheit(), dataType, dataType)
    val collector = Collector("SERVER_1", dataType)

    val l1 = Flow(temp.output, process.getInput("celsius"))
    val l4 = Flow(process.getOutput("fahrenheit"), collector.input)

    new Policy(s"Implem_TEMP_$office").add(temp).add(process).add(collector).add(l1).add(l4)

  }
}
object OfficeBuilder extends DEPOSIT {
  def apply(office:String):Policy = {

    val dataType = classOf[SmartCampusType]
    val door = EventSensor(s"DOOR_$office", dataType)
    val window = EventSensor(s"WINDOW_$office", dataType)
    val ac = PeriodicSensor(ExperimentalValues.PERIOD, s"AC_$office", dataType)
    val process = Process(OfficePolicy(), dataType, dataType)
    val collector = Collector("SERVER_1", dataType)

    val l1 = Flow(door.output, process.getInput("DOOR"))
    val l2 = Flow(window.output, process.getInput("WINDOW"))
    val l3 = Flow(ac.output, process.getInput("AC"))
    val l4 = Flow(process.getOutput("RESULT"), collector.input)

    new Policy(s"Implem_ALERT_AC_$office").add(door).add(window).add(ac).add(process).add(collector).add(l1).add(l2).add(l3).add(l4)

  }
}
object SingleConverter extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)

  val t1 = System.currentTimeMillis()
  val p = OfficeConverterBuilder("401")
  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  deploy.foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }

  val t2 = System.currentTimeMillis()

  println(s"Time elapsed to deploy one policy: ${t2 - t1} ms")
}


object SingleOffice extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)

  val t1 = System.currentTimeMillis()
  val p = OfficeBuilder("401")
  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  deploy.foreach { p =>
    val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
    generator.apply(p, toFile = true)
  }

  val t2 = System.currentTimeMillis()

  println(s"Time elapsed to deploy one policy: ${t2 - t1} ms")
}

object GlobalPolicy extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)

  val t1 = System.currentTimeMillis()
  val p = GlobalSmartBuildingDemo()
  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a global policy: ${t2 - t1} ms")
}

object GlobalPolicyWithComposition extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)
  val t1 = System.currentTimeMillis()
  val p = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeBuilder(e.toString)}
  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)
   val t2 = System.currentTimeMillis()
   println(s"Time elapsed to deploy a composed policy: ${t2 - t1} ms")
}

object GlobalPolicyConverter extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)

  val t1 = System.currentTimeMillis()
  val p = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeConverterBuilder(e.toString)}

  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)


  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a global policy: ${t2 - t1} ms")
}

object GlobalPolicyConverterWithComposition extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)
  val infraModel = InfrastructureModel(topology, ExperimentalValues.STRATEGY)
  val t1 = System.currentTimeMillis()
  val p = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeConverterBuilder(e.toString)}
  println(s"Concepts before expand ${p.concepts.size}")
  val preDeploy = PreDeploy(p, topology)

  println(s"Concepts after expand ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy a composed policy: ${t2 - t1} ms")
}
object TwoPoliciesWithComposition extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)


 val policy1 = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeBuilder(e.toString)}
 val policy2 = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeConverterBuilder(e.toString)}

 val composition = policy1 ++ policy2

  val t1 = System.currentTimeMillis()

  println(s"Concepts before expend ${composition.concepts.size}")
  val preDeploy = PreDeploy(composition, topology)
  println(s"Concepts after expend ${preDeploy.concepts.size}")
  val deploy = Deploy(preDeploy, topology, ExperimentalValues.STRATEGY)

  val t2 = System.currentTimeMillis()
  println(s"Time elapsed to deploy the two policies: ${t2 - t1} ms")

}

