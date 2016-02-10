package fr.unice.modalis.cosmic.runtime

import fr.unice.modalis.cosmic.deposit.core.Policy
import fr.unice.modalis.cosmic.{ComprehensivePolicy, ComprehensivePolicy2, InfrastructureModels}
import org.specs2.mutable.{After, SpecificationWithJUnit}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 09/02/2016.
  */
class RepositoryTest extends SpecificationWithJUnit with After{


  val repo1 = Repository(InfrastructureModels.SMARTCAMPUS_Infrastructure)
  val repo2 = Repository(InfrastructureModels.SMARTCAMPUS_Infrastructure)

  "A repository manager" should {

    "register repositories" in {
      RepositoriesManager.addRepository(repo1, Some("TestRepo1"))
      RepositoriesManager.addRepository(repo2)

      RepositoriesManager.getRepository("TestRepo1") must beSome[Repository]
      RepositoriesManager.getRepository(repo2.infrastructureModel.topology.name) must beSome[Repository]
    }

    "allow the removal of a repository" in {
      RepositoriesManager.addRepository(repo1, Some("TestRepo3"))
      RepositoriesManager.removeRepository("TestRepo3")
      RepositoriesManager.getRepository("TestRepo3") must beNone
    }
  }

  "A repository" should {
    val entity = repo1.infrastructureModel.topology.resources.find(_.name equals "ARD_1_443").get
    "allow the retrieval of deployed policies per entities" in {
      repo1.updatePolicy(ComprehensivePolicy.innerPolicy(), entity)
      repo1.getPolicy(entity) must beSome(ComprehensivePolicy.innerPolicy())
    }

    "allow the updating of a deployed policy on an entity" in {
      repo1.updatePolicy(ComprehensivePolicy.innerPolicy(), "ARD_1_443")
      repo1.updatePolicy(ComprehensivePolicy2.innerPolicy(), "ARD_1_443")
      repo1.getPolicy("ARD_1_443") must beSome(ComprehensivePolicy2.innerPolicy())
    }

    "allow the retrieval of a deployed policy on an entity" in {
      repo1.updatePolicy(ComprehensivePolicy.innerPolicy(), "ARD_1_443")
      repo1.getPolicy("ARD_1_443") must beSome[Policy]
      repo1.getPolicy(entity) must beSome[Policy]
      repo1.getPolicy(entity) must beEqualTo(repo1.getPolicy("ARD_1_443"))
    }


  }

  override def after: Any = {
    RepositoriesManager.removeRepository("TestRepo1")
    RepositoriesManager.removeRepository(repo2.infrastructureModel.topology.name)
    RepositoriesManager.removeRepository("TestRepo3")
  }

}
