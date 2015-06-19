package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.PreDeploy
import fr.unice.modalis.cosmic.deployment.utils.InfrastructureModelBuilder
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 17/06/15.
 */
object ACWhenWindowAndDoorOpened {


  val virtualState = {
    val sensor = new fr.unice.modalis.cosmic.deposit.core.EventSensor[SmartCampusType]("sensor")
    val cond = new Conditional[SmartCampusType]("i.v < 500")

    val open = new Collector[SmartCampusType]("open")
    val closed = new Collector[SmartCampusType]("closed")

    val l1 = Link(sensor.output, cond.input)
    val l2 = Link(cond.thenOutput, open.input)
    val l3 = Link(cond.elseOutput, closed.input)

    new Policy().add(sensor).add(cond).add(open).add(closed).addLink(l1).addLink(l2).addLink(l3)

  }

  val acSensor = PeriodicSensor[SmartCampusType](10, "AC_443")
  val windowSensor = EventSensor[SmartCampusType]("WINDOW_443")
  val doorSensor = EventSensor[SmartCampusType]("DOOR_443")

  val windowOpened = Process[SmartCampusType, SmartCampusType](virtualState)
  val doorOpened = Process[SmartCampusType, SmartCampusType](virtualState)
  val acOn = new Conditional[SmartCampusType]("i.v < 20")

  val switch = new Switch[SmartCampusType](Map("i1" -> "o"), Set("i1", "i2", "i3"), Set("o"))

  val collector = new Collector[SmartCampusType]("collector")


  val l1 = Link(acSensor.output, acOn.input)
  val l2 = Link(windowSensor.output, windowOpened.getInput("sensor"))
  val l3 = Link(doorSensor.output, doorOpened.getInput("sensor"))

  val l4 = Link(acOn.thenOutput, switch.getInput("i1"))
  val l5 = Link(doorOpened.getOutput("open"), switch.getInput("i2"))
  val l6 = Link(windowOpened.getOutput("open"), switch.getInput("i3"))
  val l7 = Link(switch.getOutput("o"), collector.input)
  val policy = new Policy().add(acSensor).add(windowSensor).add(doorSensor).add(windowOpened).add(doorOpened).add(acOn).add(switch).add(collector)
  .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5).addLink(l6).addLink(l7)

}

object TestDeployment extends App {
  val model = InfrastructureModelBuilder("configurations/smartcampus_demo.xml")
  model.resources.foreach {o => println(o + ": " + o.properties)}


  PreDeploy(ACWhenWindowAndDoorOpened.policy, model)
  ACWhenWindowAndDoorOpened.policy.operations.foreach(o => println(o + ":" + o.readProperty("targets")))


}

