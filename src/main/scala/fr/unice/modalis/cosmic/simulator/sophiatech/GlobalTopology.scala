package fr.unice.modalis.cosmic.simulator.sophiatech

import java.io.File

import fr.unice.modalis.cosmic.deployment.AutoDeploy
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deposit.algo.{ExtendPolicy, Unification, Weave}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import fr.unice.modalis.cosmic.simulator.smartbuilding.{OfficeBuilder, ParkingSpaceMonitoringBuilder}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 04/04/2016.
  */

object InfoScreenPolicyBuilder {

  def apply(range: Range) = {

    val sensors = for (park <- range) yield EventSensor(s"PRK_$park", classOf[SmartCampusType])
    val collector = Collector("SERVER_2", classOf[SmartCampusType])

    val adder = Add((1 to range.size).map{i => s"i$i"}.toSet, classOf[SmartCampusType], rename = Some(s"SCREEN_${range.min}_${range.max}"))

    val flows = (for ((s, idx) <- sensors.zipWithIndex) yield Flow(s.output, adder.getInput(s"i${idx+1}"))).toSet + Flow(adder.output, collector.input)

    Policy(s"InfoScreen${range.min}_${range.max}",sensors.toSet + collector, Set(adder), flows.asInstanceOf[Set[Flow[_<:DataType]]])

  }

}

object CountPlacesBuilder {

  def apply(policies : Set[Policy]) = {
    val composed = policies.reduceLeft(_ + _)
    val extended = ExtendPolicy(composed.select(composed.concepts.collect {case x:Sensor[_] => x
                                                case x:Add[_] => x}, s"select_${composed.name}"), onlyEmptyPorts = true)

    val newPolicy = {
      val adder = new Add((1 to policies.size).map {i => s"i$i"} toSet, classOf[SmartCampusType], rename = Some("COUNT_CAMPUS"))
      val output = Collector("SERVER_1", classOf[SmartCampusType])
      ExtendPolicy(new Policy("Counter").add(adder).add(output).add(new Flow(adder.output, output.input)), onlyEmptyPorts = true)
    }

    Weave(extended, newPolicy, for (i <- extended.outputJoinPoints.asInstanceOf[Set[JoinPointOutput[DataType]]] zip newPolicy.inputJoinPoints.asInstanceOf[Set[JoinPointInput[DataType]]]) yield Unification(i._1, i._2))

  }

}

object GlobalTopology extends App {

  val parkingA = InfoScreenPolicyBuilder(1 to 100)
  val parkingB = InfoScreenPolicyBuilder(101 to 200)
  val parkingC = InfoScreenPolicyBuilder(201 to 300)
  val parkingD = InfoScreenPolicyBuilder(301 to 400)
  val counter = CountPlacesBuilder(Set(parkingA, parkingB, parkingC, parkingD))
  val parkingSpaceMonitor = (for (i<- 1 to 400) yield ParkingSpaceMonitoringBuilder(i.toString)).reduceLeft(_ + _)
  val officesAlertAC = (for (i <- 1 to 500) yield OfficeBuilder(i.toString)).reduceLeft(_ + _)



  //val bigpolicy = parkingA ++ parkingB ++ parkingC ++ parkingD ++ counter ++ officesAlertAC//++ parkingSpaceMonitor ++


  val topologyModel = TopologyModelBuilder.loadFromSpineFM(xml.XML.loadFile(new File("assets/configurations/SophiaTech.xml")))

  val model = InfrastructureModel(topologyModel, DeploymentRepartition.CLOSEST_TO_SENSORS)
  val repo = RepositoriesManager.addRepository(new Repository(model))

  val tbegin = System.currentTimeMillis()
  AutoDeploy(parkingA, model)
  println("1. Done")
  AutoDeploy(parkingB, model)
  println("2. Done")
  AutoDeploy(parkingC, model)
  println("3. Done")
  AutoDeploy(parkingD, model)
  println("4. Done")
  AutoDeploy(counter, model)
  println("5. Done")
  AutoDeploy(officesAlertAC, model)
  println("6. Done")
  AutoDeploy(parkingSpaceMonitor, model)
  println("7. Done")
  val tend = System.currentTimeMillis()
  println(s"Time elapsed ${tend - tbegin} ms")

}

