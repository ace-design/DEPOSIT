package fr.unice.modalis.cosmic.workflow.scenarios

import fr.unice.modalis.cosmic.workflow.converter.ToGraphviz
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Info Traffic Service
 * "As an info traffic service
 * I want to know the less congested road
 * in order to help car drivers"
 *
 * Hypothesis 1) = The less congested road is the road with the highest average speed
 * Hypothesis 2) = The driver wants to use in priority the motorway
 * Created by Cyril Cecchinel - I3S Laboratory on 16/02/15.
 */
object InfoTrafficService2 extends App{

  val wf_TollRoad = {
    /** A8 (toll road) Sensors Antibes --> Nice **/
    val avgspeed_AntibesSophia = new EventSensor[SensorDataType]("A8Sophia")
    val avgspeed_AntibesCarrefour = new EventSensor[SensorDataType]("A8Carrefour")
    val avgspeed_AntibesBarriere = new EventSensor[SensorDataType]("A8Barriere")

    val userApplication = Collector[SensorDataType]("user")

    val lessCongestedToll = new Min[SensorDataType](Set("e1","e2","e3"))

    val l1a = new WFLink(avgspeed_AntibesBarriere.output, lessCongestedToll.getInput("e1"))
    val l1b = new WFLink(avgspeed_AntibesCarrefour.output, lessCongestedToll.getInput("e2"))
    val l1c = new WFLink(avgspeed_AntibesSophia.output, lessCongestedToll.getInput("e3"))
    val l5b = new WFLink(lessCongestedToll.output, userApplication.input)


    new Workflow("less_congested_toll").addIO(avgspeed_AntibesBarriere).addIO(avgspeed_AntibesCarrefour).addIO(avgspeed_AntibesSophia)
    .addActivity(lessCongestedToll).addIO(userApplication).addLink(l1a).addLink(l1b).addLink(l1c).addLink(l5b)
  }

  val wf_RN7 = {
    /** RN7 sensors **/
    val avgspeed_RN7MarineLand = new EventSensor[SensorDataType]("RN7MarineLand")
    val avgspeed_RN7Villeneuve = new EventSensor[SensorDataType]("RN7Villeneuve")

    val avgSpeedRN7 = new Average[SensorDataType](Set("e1","e2"))

    val l2a = new WFLink(avgspeed_RN7MarineLand.output, avgSpeedRN7.getInput("e1"))
    val l2b = new WFLink(avgspeed_RN7Villeneuve.output, avgSpeedRN7.getInput("e2"))

    val userApplication = Collector[SensorDataType]("user")

    val l3 = new WFLink(avgSpeedRN7.output, userApplication.input)

    new Workflow("average_RN7").addIO(avgspeed_RN7MarineLand).addIO(avgspeed_RN7Villeneuve).addActivity(avgSpeedRN7).addIO(userApplication).addLink(l2a).addLink(l2b).addLink(l3)

  }


  println(ToGraphviz(wf_TollRoad))



}
