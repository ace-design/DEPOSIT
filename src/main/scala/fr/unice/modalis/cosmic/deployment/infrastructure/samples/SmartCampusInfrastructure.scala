package fr.unice.modalis.cosmic.deployment.infrastructure.samples

import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 08/01/2016.
  */
object SmartCampusInfrastructure {

  def apply() = new InfrastructureModel(TopologyModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml"), DeploymentRepartition.CLOSEST_TO_SENSORS)

}
