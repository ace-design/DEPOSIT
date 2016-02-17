package fr.unice.modalis.cosmic

import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * This is a specification test of a data collection policy
  * defined with the DEPOSIT DSL
  * Created by Cyril Cecchinel - I3S Laboratory on 20/01/2016.
  */
object InfrastructureModels {

  val SMARTCAMPUS_Infrastructure = InfrastructureModel(TopologyModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml"), DeploymentRepartition.CLOSEST_TO_SENSORS)

}
object ComprehensivePolicy extends DEPOSIT {

  this hasForName "DemoPolicy"
  this handles classOf[SmartCampusType]


  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 300
  val window_sensor = declare anEventSensor() named "WINDOW_443"
  val door_sensor = declare anEventSensor() named "DOOR_443"

  val celsiusToFahrenheit = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
  val openingConverterForWindow = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val openingConverterForDoor = define aProcess StandardizedPolicies.RawValueToOpeningSensor()

  val threshold = define aFilter "value < 64"
  val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3")

  val collector = declare aCollector() named "SmartCampus"

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

object ComprehensivePolicy2 extends DEPOSIT {
  this hasForName "DemoPolicy2"
  this handles classOf[SmartCampusType]

  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 200
  val celsiusToFahrenheit = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
  val collector = declare aCollector() named "DataWarehouse"

  flows {
    ac_443() -> celsiusToFahrenheit("celsius")
    celsiusToFahrenheit("fahrenheit") -> collector()
  }

  def innerPolicy() = policy
}

object NonValidPolicy extends DEPOSIT {
  this hasForName "NonValidPolicy"
  this handles classOf[SmartCampusType]

  val testSensor = declare aPeriodicSensor() named "Test" withPeriod 3
  val testSensor2 = declare aPeriodicSensor() named "Test2" withPeriod 5
  val collector = declare aCollector() named "Collector"

  val op1 = define aFilter "value < 5"
  val op2 = define anIncrementBy IntegerType(3)

  flows {
    testSensor() -> op1("input")
    testSensor2() -> op1("input") //Whoups two data flows connected to the same port
    op1("then") -> op2("input")
    op2("output") -> collector()
  }

  def innerPolicy() = policy
}

object NonValidPolicy2 extends DEPOSIT {
  this hasForName "NonValidPolicy2"
  this handles classOf[SmartCampusType]

  val testSensor = declare aPeriodicSensor() named "Test" withPeriod 3
  val collector = declare aCollector() named "Collector"

  val op1 = define aFilter "value < 5"
  val op2 = define anIncrementBy IntegerType(3)
  val op3 = define anAdder() withInputs ("i1","i2")

  flows {
    testSensor() -> op1("input")
    op1("then") -> op2("input")
    op2("output") -> op3("i1") //Whoups i2 is not connected
    op3("output") -> collector()
  }

  def innerPolicy() = policy
}

object ComprehensivePolicyWithoutDSL {
  val ac443 = PeriodicSensor(300, "AC_443", classOf[SmartCampusType])

  val door443 = EventSensor("DOOR_443", classOf[SmartCampusType])
  val window443 = EventSensor("WINDOW_443", classOf[SmartCampusType])
  val temp_filter = Conditional("value < 18", classOf[SmartCampusType])
  val door_filter = Conditional("value < 500", classOf[SmartCampusType])
  val window_filter = Conditional("value < 500", classOf[SmartCampusType])
  val produce1 = Produce(Set("i1", "i2"), new SmartCampusType("ALERT_AC", 1), None, classOf[SmartCampusType], classOf[SmartCampusType])
  val produce2 = produce1.duplicate
  val produce3 = Produce(Set("i1", "i2"), new SmartCampusType("ALERT_AC", 0), None, classOf[SmartCampusType], classOf[SmartCampusType])
  val collector = Collector("collector", classOf[SmartCampusType])
  val collector2 = collector.duplicate
  val collector3 = collector.duplicate
  val l1 = Flow(ac443.output, temp_filter.input)
  val l2 = Flow(door443.output, door_filter.input)
  val l3 = Flow(window443.output, window_filter.input)
  val l4 = Flow(temp_filter.thenOutput, produce1.getInput("i1"))
  val l5 = Flow(door_filter.thenOutput, produce1.getInput("i2"))
  val l6 = Flow(window_filter.thenOutput, produce2.getInput("i1"))
  val l7 = Flow(temp_filter.thenOutput, produce2.getInput("i2"))
  val l8 = Flow(produce1.output, collector.input)
  val l9 = Flow(produce2.output, collector2.input)
  val l10 = Flow(door_filter.elseOutput, produce3.getInput("i1"))
  val l11 = Flow(window_filter.elseOutput, produce3.getInput("i2"))
  val l12 = Flow(produce3.output, collector3.input)


  val p = new Policy("DemoPolicywithoutDSL").add(window443).add(ac443).add(collector).add(collector2).add(collector3).add(door443)
    .add(temp_filter).add(door_filter).add(window_filter).add(produce1).add(produce2).add(produce3)
    .addFlow(l1).addFlow(l2).addFlow(l3).addFlow(l4).addFlow(l5).addFlow(l6).addFlow(l7).addFlow(l8).addFlow(l9)
    .addFlow(l10).addFlow(l11).addFlow(l12)
}

