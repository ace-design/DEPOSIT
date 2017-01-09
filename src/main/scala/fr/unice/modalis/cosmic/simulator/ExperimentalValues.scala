package fr.unice.modalis.cosmic.simulator

import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */
protected object ExperimentalValues {
  val RANGE_OFFICE = 1 to 50
  val RANGE_PRK = 1 to 500
  val STRATEGY = DeploymentRepartition.CLOSEST_TO_SENSORS
  val PERIOD = 300
  val INFRA_XML = "assets/configurations/Large3.xml"
}
