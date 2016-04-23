package fr.unice.modalis.cosmic.simulator.sophiatech

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import fr.unice.modalis.cosmic.deployment.AutoDeploy
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deposit.core.Policy
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import fr.unice.modalis.cosmic.simulator.smartbuilding.{OfficeBuilder, OfficeConverterBuilder, ParkingSpaceMonitoringBuilder}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 25/03/2016.
  */
object Benchmark extends App{

  var valuesRun = List[(Int,List[(Int,Long)])]()

  for (exec <- 1 to 10) {
    println(s"*** Execution #$exec ***")

    var xpResults = List[(Int,Long)]()

    for (i <- 100 to 1000 by 100){
      val RANGE_OFFICE = 1 to i
      val RANGE_PRK = 1 to i/2

      val policy1 = RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeBuilder(e.toString)}
      val policy2 = RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeConverterBuilder(e.toString)}
      val policy3 = RANGE_PRK.foldLeft(new Policy("")) { (acc, e) => acc ++ ParkingSpaceMonitoringBuilder(e.toString)}

      println(s"#Sensors $i")
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
      RepositoriesManager.addRepository(Repository(infra_model))

      val t1 = System.currentTimeMillis()
      AutoDeploy(policy1, infra_model)
      AutoDeploy(policy2, infra_model)
      AutoDeploy(policy3, infra_model)
      val t2 = System.currentTimeMillis()
      println(s"Deployment time for 3 policies ($i sensors): ${t2 - t1} ms")
      xpResults = (i, t2 - t1) :: xpResults
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
