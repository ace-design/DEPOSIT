package fr.unice.modalis.cosmic.simulator

import fr.unice.modalis.cosmic.deposit.core.Policy.NonValidPolicyException
import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 18/02/2016.
  */
class RandomPolicyTest extends SpecificationWithJUnit{

  "A random policy generator" should {
    "produce valid policies" in {
      val nbConcepts = 5 + scala.util.Random.nextInt(20)
      val maxDegree = nbConcepts / 3
      Policy.checkValidity(RandomPolicy(nbConcepts, maxDegree)) must not(throwA[NonValidPolicyException])
    }
    "check the user inputs" in {
      RandomPolicy(-2, 3) must throwAn[IllegalArgumentException]
      RandomPolicy(3, 0) must throwAn[IllegalArgumentException]
    }
  }

  "Simulated concepts factory" should {
    "provide simulated sensors" in {
      SimulatedConceptFactory(0, 5, classOf[SmartCampusType]) must beAnInstanceOf[Sensor[_]]
      SimulatedConceptFactory(4, 0, classOf[SmartCampusType]) must beAnInstanceOf[Collector[_]]
      SimulatedConceptFactory(3, 2, classOf[SmartCampusType]) must beAnInstanceOf[Concept]
    }
  }

}
