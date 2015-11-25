package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deployment.generator.{ArduinoGenerator, BRGenerator}
import fr.unice.modalis.cosmic.deployment.heuristics.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.InfrastructureModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 13/11/2015.
  */
object DemoAlertACv4 extends App{

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
    *           Process reuse
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

  // We use a standarized policy to assess the window/door state
  val temp_filter = Conditional("value < 18", classOf[SmartCampusType])
  val door_process = Process(StandardizedPolicies.rawValueToOpeningSensor, classOf[SmartCampusType], classOf[SmartCampusType])
  val window_process = Process(StandardizedPolicies.rawValueToOpeningSensor, classOf[SmartCampusType], classOf[SmartCampusType])

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
  val l1 = Link(ac443.output, temp_filter.input)
  val l2 = Link(door443.output, door_process.getInput("input"))
  val l3 = Link(window443.output, window_process.getInput("input"))
  val l4 = Link(temp_filter.thenOutput, produce1.getInput("i1"))
  val l5 = Link(door_process.getOutput("open"), produce1.getInput("i2"))
  val l6 = Link(window_process.getOutput("open"), produce2.getInput("i1"))
  val l7 = Link(temp_filter.thenOutput, produce2.getInput("i2"))
  val l8 = Link(produce1.output, collector.input)
  val l9 = Link(produce2.output, collector2.input)
  val l10 = Link(door_process.getOutput("closed"), produce3.getInput("i1"))
  val l11 = Link(window_process.getOutput("closed"), produce3.getInput("i2"))
  val l12 = Link(produce3.output, collector3.input)

  // We build the "ALERT_AC2" data collection policy
  val p = new Policy("ALERT_AC2").add(window443).add(ac443).add(collector).add(collector2).add(collector3).add(door443)
    .add(temp_filter).add(door_process).add(window_process).add(produce1).add(produce2).add(produce3)
    .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5).addLink(l6).addLink(l7).addLink(l8).addLink(l9)
    .addLink(l10).addLink(l11).addLink(l12)

  ToGraphviz.writeSource(p)
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



  ToGraphviz.writeSource(policyArd1)
  ToGraphviz.writeSource(policyArd2)
  ToGraphviz.writeSource(policyRp)


  ArduinoGenerator(policyArd1, toFile = true)
  ArduinoGenerator(policyArd2, toFile = true)
  BRGenerator(policyRp, toFile = true)

}