package fr.unice.modalis.cosmic.workflow.scenarios

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Info Trafic Service
 * "As an info trafic service
 * I want to know the less congested road
 * in order to help car drivers"
 *
 * Hypothesis 1) = The less congested road is the road with the highest average speed
 * Hypothesis 2) = The driver wants to use in priority the motorway
 * Created by Cyril Cecchinel - I3S Laboratory on 16/02/15.
 */
object InfoTrafficService extends App{

  /** A8 (toll road) Sensors Antibes --> Nice **/
  val avgspeed_AntibesSophia = new EventSensor[SensorDataType]("A8Sophia")
  val avgspeed_AntibesCarrefour = new EventSensor[SensorDataType]("A8Carrefour")
  val avgspeed_AntibesBarriere = new EventSensor[SensorDataType]("A8Barriere")

  /** RN7 sensors **/
  val avgspeed_RN7MarineLand = new EventSensor[SensorDataType]("RN7MarineLand")
  val avgspeed_RN7Villeneuve = new EventSensor[SensorDataType]("RN7Villeneuve")

  /** Seefront sensor **/
  val avgspeed_SeefrontSiesta = new EventSensor[SensorDataType]("SFSiesta")
  val avgspeed_SeefrontMarina = new EventSensor[SensorDataType]("SFMarina")


  val userApplication = Collector[SensorDataType]("user")

  /**
   * Activities definition
   */

  /**
   * A1. Determine the less congested toll (motorway entrance)
   */

  val lessCongestedToll = new Min[SensorDataType](Set("e1","e2","e3"))

  /**
   * A2. Compute the average speed on the RN7 road (multi-entrance)
   */

  val avgSpeedRN7 = new Average[SensorDataType](Set("e1","e2"))


  /**
   * A3. Compute the average speed on the Seefront road (mutli-entrance)
   */

  val avgSpeedSeeFront = new Average[SensorDataType](Set("e1","e2"))

  /**
   * A4. Determine the less congested road between RN7 and Seefront
   */

  val lessCongestedRoad = new Min[SensorDataType](Set("e1","e2"))

  /**
   * A5. Determine the less congested road
   */

  val lessCongested= new Min[SensorDataType](Set("e1","e2"))


  /**
   * Link definitions
   */

  /**
   * L1. Determine the less congested toll (motorway entrance)
   */

  val l1a = new WFLink(avgspeed_AntibesBarriere.output, lessCongestedToll.getInput("e1").get)
  val l1b = new WFLink(avgspeed_AntibesCarrefour.output, lessCongestedToll.getInput("e2").get)
  val l1c = new WFLink(avgspeed_AntibesSophia.output, lessCongestedToll.getInput("e3").get)

  /**
   * L2. Compute the average speed on the RN7 road (multi-entrance)
   */

  val l2a = new WFLink(avgspeed_RN7MarineLand.output, avgSpeedRN7.getInput("e1").get)
  val l2b = new WFLink(avgspeed_RN7Villeneuve.output, avgSpeedRN7.getInput("e2").get)

  /**
   * L3. Compute the average speed on the Seefront road (mutli-entrance)
   */

  val l3a = new WFLink(avgspeed_SeefrontMarina.output, avgSpeedRN7.getInput("e1").get)
  val l3b = new WFLink(avgspeed_SeefrontSiesta.output, avgSpeedRN7.getInput("e2").get)

  /**
   * L4. Determine the less congested road between RN7 and Seefront
   */

  val l4a = new WFLink(avgSpeedRN7.output, lessCongestedRoad.getInput("e1").get)
  val l4b = new WFLink(avgSpeedSeeFront.output, lessCongestedRoad.getInput("e2").get)

  /**
   * L5. Determine the less congested road
   */

  val l5a = new WFLink(lessCongestedRoad.output, lessCongested.getInput("e1").get)
  val l5b = new WFLink(lessCongestedToll.output, lessCongested.getInput("e2").get)

  /**
   * L6. Send data to user
   */
  val l6 = new WFLink(lessCongested.output, userApplication.input)

  /**
   * Add activities & links in the workflow
   */
  val workflow = new Workflow().addIO(avgspeed_AntibesBarriere).addIO(avgspeed_AntibesCarrefour).addIO(avgspeed_AntibesSophia).addIO(avgspeed_RN7MarineLand).addIO(avgspeed_RN7Villeneuve).addIO(avgspeed_SeefrontMarina).addIO(avgspeed_SeefrontSiesta)
    .addActivity(lessCongestedRoad).addActivity(avgSpeedRN7).addActivity(avgSpeedSeeFront).addActivity(lessCongestedToll).addLink(l1a).addLink(l1b).addLink(l1c).addLink(l2a).addLink(l2b).addLink(l3a).addLink(l3b).addLink(l4a).addLink(l4b).addLink(l5a).addLink(l5b).addLink(l6)

  println(workflow)

}
