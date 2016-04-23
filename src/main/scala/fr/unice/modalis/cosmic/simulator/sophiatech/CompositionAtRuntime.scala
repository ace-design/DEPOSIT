package fr.unice.modalis.cosmic.simulator.sophiatech

import fr.unice.modalis.cosmic.deployment.AutoDeploy
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deposit.core.Policy
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import fr.unice.modalis.cosmic.simulator.ExperimentalValues
import fr.unice.modalis.cosmic.simulator.smartbuilding.{OfficeBuilder, OfficeConverterBuilder, ParkingSpaceMonitoringBuilder}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 20/04/2016.
  */
object CompositionAtRuntime extends App {
  val topology = TopologyModelBuilder(ExperimentalValues.INFRA_XML)
  val model = InfrastructureModel(topology, ExperimentalValues.STRATEGY)

  RepositoriesManager.addRepository(Repository(model))
  val policy1 = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeBuilder(e.toString)}
  val policy2 = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ OfficeConverterBuilder(e.toString)}
  val policy3 = ExperimentalValues.RANGE_PRK.foldLeft(new Policy("")) { (acc, e) => acc ++ ParkingSpaceMonitoringBuilder(e.toString)}

  val tbegin = System.currentTimeMillis()
  AutoDeploy(policy1, model)
  val tinter1 = System.currentTimeMillis()
  println(s"Time elapsed for policy 1 ${tinter1 - tbegin} ms")

  val tbegin2 = System.currentTimeMillis()
  AutoDeploy(policy2, model)
  val tinter2 = System.currentTimeMillis()
  println(s"Time elapsed for policy 2 ${tinter2 - tbegin2} ms")

  val tbegin3 = System.currentTimeMillis()
  AutoDeploy(policy3, model)
  val tinter3 = System.currentTimeMillis()
  println(s"Time elapsed for policy 3 ${tinter3 - tbegin3} ms")


  val tend = System.currentTimeMillis()
  println(s"Time elapsed ${tend - tbegin} ms")
  println(s"Number of updates: ${RepositoriesManager.getRepository(topology.name).get.nbUpdates}")

}
