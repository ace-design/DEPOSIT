package fr.unice.modalis.cosmic.deposit.core

import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 05/05/15.
 */
class UtilsTest extends SpecificationWithJUnit{

  "Utils methods" should {
    "Create n instances of Event sensors" in {
      Utils.createEventSensors[IntegerType]("test_",10) must have size 10
    }
    "Create n instances of collector" in {
      Utils.createCollectors[IntegerType]("test_",10) must have size 10
    }
    "Create n instances of Periodic sensors" in {
      val res = Utils.createPeriodicSensors[IntegerType](5, "test_",10)
      "check n instances" in {
        res must have size 10
      }
      "check period" in {
        res.forall(_.wishedPeriod == 5) must beTrue
      }
    }
  }

}
