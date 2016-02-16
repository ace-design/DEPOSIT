package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deployment.infrastructure.samples.SmartCampusInfrastructure
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{AutoDeploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}


/**
  * This demonstration illustrates how a process can be used to convert
  * a celsius temperature into a fahrenheit temperature
  *
  * Concepts demonstrated :
  *           Process reuse
  *
  * Created by Cyril Cecchinel - I3S Laboratory on 18/11/2015.
  */
object DemoCelsiusToFahrenheit extends App{

  val sensor = PeriodicSensor(2, "AC_443", classOf[SmartCampusType])
  val convert = Process(StandardizedPolicies.CelsiusToFahrenheit(), classOf[SmartCampusType], classOf[SmartCampusType])
  val collectorfahrenheit = Collector("SmartCampus", classOf[SmartCampusType])
  val collectorcelsius = Collector("SmartCampus", classOf[SmartCampusType])

  val l1 = Flow(sensor.output, convert.getInput("celsius"))
  val l2 = Flow(convert.getOutput("fahrenheit"), collectorfahrenheit.input)
  val l3 = Flow(sensor.output, collectorcelsius.input)

  val p = new Policy("CelsiusToFarenheit")
    .add(sensor).add(convert).add(collectorcelsius).add(collectorfahrenheit)
    .addFlow(l1).addFlow(l2).addFlow(l3)

  val topology = TopologyModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml")
  val predeployed = PreDeploy(p, topology)

  RepositoriesManager.addRepository(new Repository(SmartCampusInfrastructure()))
  AutoDeploy(p, SmartCampusInfrastructure())


}
