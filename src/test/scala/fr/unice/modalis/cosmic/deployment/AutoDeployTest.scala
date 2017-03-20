package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.demos.DemoAlertACWithDSL
import fr.unice.modalis.cosmic.deployment.infrastructure.samples.SmartCampusInfrastructure
import fr.unice.modalis.cosmic.deposit.algo.FactorizePolicy
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, Repository}
import org.specs2.mutable.SpecificationWithJUnit

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 20/03/2017.
  */
class AutoDeployTest extends SpecificationWithJUnit{

  val infrastructureModel = SmartCampusInfrastructure()
  RepositoriesManager.addRepository(new Repository(infrastructureModel))
  val policyToDeploy = DemoAlertACWithDSL()
  AutoDeploy(policyToDeploy, infrastructureModel)

  "The auto-deployement of a policy" should {
    val repository = RepositoriesManager.getRepository(infrastructureModel.topology.name).get

    "generate the sub-policies" in {
      repository.getPolicies.keySet must haveSize(3)
    }

    "respect the deployment strategy" in {
      repository.getPolicy("ARD_1_443") should beSome
      FactorizePolicy(repository.getPolicy("ARD_1_443").get).concepts must haveSize(2)

      repository.getPolicy("ARD_2_443") should beSome
      FactorizePolicy(repository.getPolicy("ARD_2_443").get).concepts must haveSize(4)

      repository.getPolicy("RP_443_XBEE") should beSome
      FactorizePolicy(repository.getPolicy("RP_443_XBEE").get).concepts must haveSize(2)
    }

    "have join points" in {
      val bridgeInputsJPnames = repository.getPolicy("RP_443_XBEE").get.inputJoinPoints.map{_.readProperty("network").get.asInstanceOf[String]}
      val policyOutputsJPnames = repository.getPolicy("ARD_1_443").get.outputJoinPoints.map{_.readProperty("network").get.asInstanceOf[String]}
      bridgeInputsJPnames must contain (policyOutputsJPnames)
    }



  }



}
