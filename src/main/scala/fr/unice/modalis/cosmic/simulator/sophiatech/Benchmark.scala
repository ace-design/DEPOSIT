package fr.unice.modalis.cosmic.simulator.sophiatech

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core.Policy
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import fr.unice.modalis.cosmic.simulator.smartbuilding.{OfficeBuilder, OfficeConverterBuilder, ParkingSpaceMonitoringBuilder}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 25/03/2016.
  */
object Benchmark extends App{

  var valuesRun = List[(Int,List[(Int,Long)])]()

  for (exec <- 1 to 1) {
    println(s"*** Execution #$exec ***")

    var xpResults = List[(Int,Long)]()

    for (i <- 100 to 1000 by 100){
      val RANGE_OFFICE = 1 to i
      val RANGE_PRK = 1 to i/2

      val policy1 = RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc + OfficeBuilder(e.toString)}
      val policy2 = RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc + OfficeConverterBuilder(e.toString)}
      val policy3 = RANGE_PRK.foldLeft(new Policy("")) { (acc, e) => acc + ParkingSpaceMonitoringBuilder(e.toString)}

      val NB_SENSORS = i * 3 + i/2

      println(s"#Sensors $NB_SENSORS")
      val model = GlobalTopologyGenerator("SophiaTech",
        nbOffices = i,
        nbParkingDistrict = 5,
        maxStageLevel = 2,
        maxRelayPerStage = 3,
        maxGateway = 2,
        maxServer = 2,
        parkingSpacesPerDistrict = i/2)
      scala.xml.XML.save("assets/configurations/demo_smartcampus.xml", model)
      val infra_model = InfrastructureModel(TopologyModelBuilder.loadFromSpineFM("assets/configurations/demo_smartcampus.xml"), DeploymentRepartition.CLOSEST_TO_SENSORS)
      val repo = Repository(infra_model)
      RepositoriesManager.addRepository(repo)

      val t1a = System.currentTimeMillis()
      Deploy(PreDeploy(policy1, infra_model.topology), infra_model.topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
      val t2a = System.currentTimeMillis()
      println(s"Deployment p1 ($NB_SENSORS sensors): ${t2a - t1a} ms")

      val t1b = System.currentTimeMillis()
      Deploy(PreDeploy(policy2, infra_model.topology), infra_model.topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
      val t2b = System.currentTimeMillis()
      println(s"Deployment p2 ($NB_SENSORS sensors): ${t2b - t1b} ms")

      val t1c = System.currentTimeMillis()
      Deploy(PreDeploy(policy3, infra_model.topology), infra_model.topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
      val t2c = System.currentTimeMillis()
      println(s"Deployment p3 ($NB_SENSORS sensors): ${t2c - t1c} ms")



      //println(s"Deployment time for 3 policies ($i sensors): ${t2 - t1} ms")
      //xpResults = (i, t2 - t1) :: xpResults
      new File("assets/configurations/demo_smartcampus.xml").delete()
    }
    xpResults = xpResults.reverse
    valuesRun = (exec, xpResults) :: valuesRun

  }

  valuesRun = valuesRun.reverse
  val f = new File(s"out/benchmark/result_xp_smartcampus.csv")
  val writer = CSVWriter.open(f)
  valuesRun.foreach(v => writer.writeRow(v._2.map(_._2)))
  writer.close()

}
