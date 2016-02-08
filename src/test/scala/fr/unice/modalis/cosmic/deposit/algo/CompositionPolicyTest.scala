package fr.unice.modalis.cosmic.deposit.algo

import fr.unice.modalis.cosmic.deposit.core.SmartCampusType
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import org.specs2.mutable.SpecificationWithJUnit

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 08/02/2016.
  */

object P1 extends DEPOSIT {
  this handles classOf[SmartCampusType]
  val sensorA = declare anEventSensor() named "A"
  val sensorB = declare aPeriodicSensor() named "B" withPeriod 10

  val collector = declare aCollector() named "C"

  val produce = define aProducer new SmartCampusType("TEST", 0) withInputs("i1", "i2")

  flows {
    sensorA() -> produce("i1")
    sensorB() -> produce("i2")
    produce("output") -> collector()
  }

  val innerPolicy = this.policy
}

object P2 extends DEPOSIT {
  this handles classOf[SmartCampusType]
  val sensorA = declare anEventSensor() named "A"

  val collector = declare aCollector() named "C"


  flows {
    sensorA() -> collector()
  }

  val innerPolicy = this.policy
}

object P3 extends DEPOSIT {
  this handles classOf[SmartCampusType]
  val sensorA = declare aPeriodicSensor() named "A" withPeriod 20

  val collector = declare aCollector() named "C"

  flows {
    sensorA() -> collector()
  }

  val innerPolicy = this.policy
}

class CompositionPolicyTest extends SpecificationWithJUnit{
  "The composition operator" should {
    "factorize similar sensors" in {
      (P1.innerPolicy ++ P2.innerPolicy).sensors must haveSize(2)
    }

    "not factorize collector" in {
      (P1.innerPolicy ++ P2.innerPolicy).collectors must haveSize(2)
    }

    "not only rely on name to factorize sensors" in {
      (P1.innerPolicy ++ P2.innerPolicy ++ P3.innerPolicy).sensors must haveSize(3)
    }
  }
}
