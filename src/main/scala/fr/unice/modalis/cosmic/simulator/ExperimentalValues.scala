package fr.unice.modalis.cosmic.simulator

import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */
protected object ExperimentalValues {
  val RANGE_OFFICE = 300 to 400
  val RANGE_PRK = 1 to 300
  val STRATEGY = DeploymentRepartition.CLOSEST_TO_SENSORS
  val PERIOD = 300
  val INFRA_XML = "assets/configurations/SophiaTech.xml"
}
