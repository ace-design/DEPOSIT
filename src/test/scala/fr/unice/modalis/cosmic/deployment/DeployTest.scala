package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
class DeployTest extends SpecificationWithJUnit{

  val s = EventSensor("parking_sensor", classOf[SantanderParkingType])
  val s2 = EventSensor("parking_sensor2", classOf[IntegerType])

  val constant = Constant(new IntegerType(1), classOf[IntegerType])
  val a1 = Extract("status", classOf[SantanderParkingType], classOf[IntegerType])
  val a2 = Conditional[IntegerType]("i == 1", classOf[IntegerType])
  val a3a = Sub[IntegerType](Set("i1", "i2"), classOf[IntegerType])
  val a3b = Add[IntegerType](Set("i1", "i2"), classOf[IntegerType])

  val place_status = Collector("place_status", classOf[IntegerType])
  val lnew = new Flow(s2.output, place_status.input)
  val l1 = new Flow(s.output, a1.input)
  val l2 = new Flow(a1.output, a2.input)
  val lnew2 = new Flow(a2.thenOutput, a2.input) // Introduce loop
  val l3a = new Flow(a2.thenOutput, a3a.getInput("i1"))
  val l3b = new Flow(constant.output, a3a.getInput("i2"))

  val l4a = new Flow(a2.elseOutput, a3b.getInput("i1"))
  val l4b = new Flow(constant.output, a3b.getInput("i2"))

  val l5a = new Flow(a3a.output, place_status.input)
  val l5b = new Flow(a3b.output, place_status.input)

  val policy = new Policy("convert", Set(s, s2,place_status), Set(a1, a2, a3a, a3b, constant), Set(lnew, l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))


  "A concept knows where data coming from" in {
    policy.sensorsInvolved(a2) mustEqual Set(s)
    policy.sensorsInvolved(place_status) mustEqual Set(s, s2)
  }

}
