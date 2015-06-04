package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */
class DeployTest extends SpecificationWithJUnit{

  val s = EventSensor[SantanderParkingType]("parking_sensor")
  val s2 = EventSensor[IntegerType]("parking_sensor2")

  val constant = Constant(new IntegerType(1))
  val a1 = Extract[SantanderParkingType, IntegerType]("status")
  val a2 = Conditional[IntegerType]("i == 1")
  val a3a = Sub[IntegerType](Set("i1", "i2"))
  val a3b = Add[IntegerType](Set("i1", "i2"))

  val place_status = Collector[IntegerType]("place_status")
  val lnew = new Link[IntegerType](s2.output, place_status.input)
  val l1 = new Link[SantanderParkingType](s.output, a1.input)
  val l2 = new Link[IntegerType](a1.output, a2.input)
  val lnew2 = new Link[IntegerType](a2.thenOutput, a2.input) // Introduce loop
  val l3a = new Link[IntegerType](a2.thenOutput, a3a.getInput("i1"))
  val l3b = new Link[IntegerType](constant.output, a3a.getInput("i2"))

  val l4a = new Link[IntegerType](a2.elseOutput, a3b.getInput("i1"))
  val l4b = new Link[IntegerType](constant.output, a3b.getInput("i2"))

  val l5a = new Link[IntegerType](a3a.output, place_status.input)
  val l5b = new Link[IntegerType](a3b.output, place_status.input)

  val policy = new Policy("convert", Set(s, s2,place_status), Set(a1, a2, a3a, a3b, constant), Set(lnew, l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))


  "A concept knows where data coming from" in {
    policy.sensorsInvolved(a2) mustEqual Set(s)
    policy.sensorsInvolved(place_status) mustEqual Set(s, s2)
  }

}
