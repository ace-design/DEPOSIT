package fr.unice.modalis.cosmic.deposit.algo

import fr.unice.modalis.cosmic.ComprehensivePolicy
import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Operation tests
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
class ExtendPolicyTest extends SpecificationWithJUnit {
  "Helper methods" should {
    "Factorize invert Extend operation" in {
      FactorizePolicy(ExtendPolicy(DCPTest.convert_workflow)) must be equalTo DCPTest.convert_workflow
    }
  }

  "An expendable policy" should {
    "provide join points for all expendable operations" in {
      ExtendPolicy(ComprehensivePolicy.innerPolicy()).ios.collect {case x:JoinPoint[_] => x} must have size 11
    }

  }

  "A factorized policy" should {
    "not contain any Join Points" in {
      FactorizePolicy(ComprehensivePolicy.innerPolicy()).ios.collect {case x:JoinPoint[_] => x} must have size 0
    }

  }
}
