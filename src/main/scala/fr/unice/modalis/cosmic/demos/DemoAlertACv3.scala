package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deployment.generator.{ProcessingGenerator, PythonGenerator}
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.InfrastructureModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 13/11/2015.
  */
object DemoAlertACv3 extends App{

  /**
    * This demonstration illustrates how three sensors can infer a new information
    * Scenario: Produce an virtual sensor "ALERT_AC" retrieving if window or door are opened
    *           while air conditioning is powered on
    *
    * Concepts demonstrated :
    *           Multi-staging (2 Arduino platform, 1 bridge)
    *           Concept repartition over an existing infrastructure
    *           Null values
    *           Deployment heuristic
    */

  /**
    * We define the sensors deployed in the monitored office
    * Each sensor has been declared in the assets/sensors/mapping.csv file
    */

  // Air conditioning temperature sensor (Period = 60s, value in celsius degrees)
  val ac443 = PeriodicSensor(60, "AC_443", classOf[SmartCampusType])

  // Door and window opening sensors (Event-based, value : < 500 Open, >= 500 Closed)
  val door443 = EventSensor("DOOR_443", classOf[SmartCampusType])
  val window443 = EventSensor("WINDOW_443", classOf[SmartCampusType])

  // We use filter operations to check if air conditioning is on (temperature < 18°C)
  // and door/window opened (value < 500 => Open)
  val temp_filter = Conditional("value < 18", classOf[SmartCampusType])
  val door_filter = Conditional("value < 500", classOf[SmartCampusType])
  val window_filter = Conditional("value < 500", classOf[SmartCampusType])

  // When all filter produce data (ie. air conditioning is on and Window/Door opened), produce
  // a data upon the SmartCampus format named ALERT_AC with value 1.
  val produce1 = Produce(Set("i1", "i2"), new SmartCampusType("ALERT_AC", 1), None, classOf[SmartCampusType], classOf[SmartCampusType])
  val produce2 = produce1.duplicate
  val produce3 = Produce(Set("i1", "i2"), new SmartCampusType("ALERT_AC", 0), None, classOf[SmartCampusType], classOf[SmartCampusType])

  // We collect the results
  val collector = Collector("collector", classOf[SmartCampusType])
  val collector2 = collector.duplicate
  val collector3 = collector.duplicate
  /**
    * We define data flows between the concepts
    */
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

  // We build the "ALERT_AC2" data collection policy
  val p = new Policy("ALERT_AC2").add(window443).add(ac443).add(collector).add(collector2).add(collector3).add(door443)
    .add(temp_filter).add(door_filter).add(window_filter).add(produce1).add(produce2).add(produce3)
    .addFlow(l1).addFlow(l2).addFlow(l3).addFlow(l4).addFlow(l5).addFlow(l6).addFlow(l7).addFlow(l8).addFlow(l9)
    .addFlow(l10).addFlow(l11).addFlow(l12)

  // We prepare the policy to be deployed over the SmartCampus infrastructure
  val topology = InfrastructureModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml")
  val predeployed = PreDeploy(p, topology)


  // We display the possible concept repartition (1) and we rely on the heuristic to identify where to deploy a concept (2)
  println("Concept repartition:")
  predeployed.concepts.foreach(concept => println("\t* " + concept + ": " + concept.readProperty("targets"))) // (1)
  val policies = Deploy(predeployed, topology, DeploymentRepartition.CLOSEST_TO_SENSORS) // (2)


  // We display available policies
  println("Available policies:")
  policies.foreach(p => println("\t" + "* " + p.name))


  val policyArd1 = policies.find(_.name equals "ALERT_AC2_ARD_1_443").getOrElse(throw new Exception("Non found policy"))
  val policyArd2 = policies.find(_.name equals "ALERT_AC2_ARD_2_443").getOrElse(throw new Exception("Non found policy"))
  val policyRp = policies.find(_.name equals "ALERT_AC2_RP_443_XBEE").getOrElse(throw new Exception("Non found policy"))


  ToGraphviz.writeSource(predeployed)
  ToGraphviz.writeSource(policyArd1)
  ToGraphviz.writeSource(policyArd2)
  ToGraphviz.writeSource(policyRp)


  ProcessingGenerator(policyArd1, toFile = true)
  ProcessingGenerator(policyArd2, toFile = true)
  PythonGenerator(policyRp, toFile = true)

}
