package fr.unice.modalis.cosmic.deposit.algo

import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Operation tests
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
class ExtendPolicyTest extends SpecificationWithJUnit {
  "Helper methods" should {
    "compute the join points and links for an operation" in {
      val adder = new Add[IntegerType](Set("i1", "i2"))
      val p = new Policy().add(adder)
      val res = ExtendPolicy.generateJoinPointsForOperation(adder, p)
      (res._1 must have size 3) and (res._2 must have size 3)
    }

    "not compute the join points for an operation if this latter is not extendable" in {
      val adder = new Add[IntegerType](Set("i1", "i2"))
      val p = new Policy().add(adder)
      adder.setExpendable(false)
      ExtendPolicy.generateJoinPointsForOperation(adder, p) must throwAn[IllegalArgumentException]
    }

    "Factorize invert Extend operation" in {
      FactorizePolicy(ExtendPolicy(DCPTest.convert_workflow)) must be equalTo DCPTest.convert_workflow
    }
  }

  "An expendable policy" should {
    "provide join points for all expendable operations" in {
      ExtendPolicy(DCPTest.convert_workflow).ios.collect {case x:JoinPoint[_] => x} must have size 12
    }
    "provide join points for all expendable operations (2)" in {
      ExtendPolicy(DCPTest.convert_workflow2).ios.collect {case x:JoinPoint[_] => x} must have size 11
    }
  }

  "A factorized policy" should {
    "not contain any Join Points" in {
      FactorizePolicy(ExtendPolicy(DCPTest.convert_workflow)).ios.collect {case x:JoinPoint[_] => x} must have size 0
    }

  }
}
