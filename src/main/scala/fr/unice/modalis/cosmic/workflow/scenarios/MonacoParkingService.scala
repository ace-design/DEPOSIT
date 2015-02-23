package fr.unice.modalis.cosmic.workflow.scenarios

import fr.unice.modalis.cosmic.workflow.converter.ToGraphviz
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 23/02/15.
 */
object MonacoParkingService extends App{

  /********* SENSORS LIST ***********/

  // Monte Carlo Casino
  val mc_p1 = new EventSensor[SensorDataType]("MC_A001")
  val mc_p2 = new EventSensor[SensorDataType]("MC_A002")
  val mc_p3 = new EventSensor[SensorDataType]("MC_A003")
  val mc_p4 = new EventSensor[SensorDataType]("MC_B001")
  val mc_p5 = new EventSensor[SensorDataType]("MC_B002")
  val mc_p6 = new EventSensor[SensorDataType]("MC_C001")
  val mc_p7 = new EventSensor[SensorDataType]("MC_C002")

  /** VIP parking spaces (Disabled Parking)**/
  val mc_p8 = new EventSensor[SensorDataType]("MC_VIP-001")

  val mc_set = Set[EventSensor[SensorDataType]](mc_p1, mc_p2, mc_p3, mc_p4, mc_p5, mc_p6, mc_p7, mc_p8)
  
  // Fontvieille Mall
  /** Regular parking spaces **/
  val ft_p1 = new EventSensor[SensorDataType]("FT_A001")
  val ft_p2 = new EventSensor[SensorDataType]("FT_A002")
  val ft_p3 = new EventSensor[SensorDataType]("FT_A003")
  val ft_p4 = new EventSensor[SensorDataType]("FT_B001")
  val ft_p5 = new EventSensor[SensorDataType]("FT_B002")

  /** Restricted parking spaces (Disabled Parking)**/
  val ft_p6 = new EventSensor[SensorDataType]("FT_RES-001")
  val ft_p7 = new EventSensor[SensorDataType]("FT_RES-002")
  
  val ft_set = Set[EventSensor[SensorDataType]](ft_p1, ft_p2, ft_p3, ft_p4, ft_p5, ft_p6, ft_p7)

  /**
   * This Workflow represents an application designed to help
   * people by giving them the amount of free parking spaces in
   * the Fontvieille Mall. We can identify two kinds of spaces :
   *    Regular spaces
   *    Disabled people spaces
   * The number of regular spaces is shown on a public screen led at the entrance of the parking and
   * at diverse locations within the principauty
   * The number of spaces reserved for disabled people are shown on a dedicated application build embedded in
   * their cars.
   */

  val parkingCCF = {


    val ledDisplay = new Collector[SensorDataType]("ledDisplayFontvieilleMall")
    val disableApplication = new Collector[SensorDataType]("disabledParkingApplication")

    /** Compute sum of free places for regular parking **/
    val freeRegular = new Sum[SensorDataType](Set("i1","i2","i3","i4","i5"))

    /** Compute sum of free places for disabled people parking **/
    val freeDisabled = new Sum[SensorDataType](Set("i1","i2"))


    val l1 = new WFLink(ft_p1.output, freeRegular.getInput("i1"))
    val l2 = new WFLink(ft_p2.output, freeRegular.getInput("i2"))
    val l3 = new WFLink(ft_p3.output, freeRegular.getInput("i3"))
    val l4 = new WFLink(ft_p4.output, freeRegular.getInput("i4"))
    val l5 = new WFLink(ft_p5.output, freeRegular.getInput("i5"))

    val l6 = new WFLink(ft_p6.output, freeDisabled.getInput("i1"))
    val l7 = new WFLink(ft_p7.output, freeDisabled.getInput("i2"))

    val l8 = new WFLink(freeDisabled.output, disableApplication.input)
    val l9 = new WFLink(freeRegular.output, ledDisplay.input)


    new Workflow("Parking Fontvieille").addIO(ft_p1).addIO(ft_p2).addIO(ft_p3).addIO(ft_p4).addIO(ft_p5).addIO(ft_p6).addIO(ft_p7)
                  .addIO(ledDisplay).addIO(disableApplication)
                  .addActivity(freeRegular).addActivity(freeDisabled)
                  .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5).addLink(l6).addLink(l7).addLink(l8).addLink(l9)
  }

  /**
   * This Workflow represents an application designed to help
   * people by giving them the amount of free parking spaces in
   * the Monte-Carlo Casino. We can identify two kinds of spaces :
   *    Regular spaces
   *    VIP People
   * The number of regular spaces is shown on a public screen led at the entrance of the parking and
   * at diverse locations within the principauty
   * The number of spaces reserved for VIP are shown in a premium application on driver's smartphones
   */
  val parkingMCCasino = {
    /** Regular parking spaces **/


    val ledDisplay = new Collector[SensorDataType]("ledDisplayMCCasino")
    val vipApplication = new Collector[SensorDataType]("VIP")

    /** Compute sum of free places for regular parking **/
    val freeRegular = new Sum[SensorDataType](Set("i1","i2","i3","i4","i5","i6","i7"))



    val l1 = new WFLink(mc_p1.output, freeRegular.getInput("i1"))
    val l2 = new WFLink(mc_p2.output, freeRegular.getInput("i2"))
    val l3 = new WFLink(mc_p3.output, freeRegular.getInput("i3"))
    val l4 = new WFLink(mc_p4.output, freeRegular.getInput("i4"))
    val l5 = new WFLink(mc_p5.output, freeRegular.getInput("i5"))
    val l6 = new WFLink(mc_p6.output, freeRegular.getInput("i6"))
    val l7 = new WFLink(mc_p7.output, freeRegular.getInput("i7"))

    val l8 = new WFLink(mc_p8.output, vipApplication.input)
    val l9 = new WFLink(freeRegular.output, ledDisplay.input)


    new Workflow("Parking Monte-Carlo").addIO(mc_p1).addIO(mc_p2).addIO(mc_p3).addIO(mc_p4).addIO(mc_p5).addIO(mc_p6).addIO(mc_p7).addIO(mc_p8)
      .addIO(ledDisplay).addIO(vipApplication)
      .addActivity(freeRegular)
      .addLink(l1).addLink(l2).addLink(l3).addLink(l4).addLink(l5).addLink(l6).addLink(l7).addLink(l8).addLink(l9)
  }


  /**
   * This Workflow is designed for an urbanist. She wants to know the amount of free parking spaces
   * in the principauty.
   * She doesn't want to take in account VIP spaces as those spaces are private and independent of the city
   * administration.
   */
  val monacoParking = {
    var wf = new Workflow()

    val parkingMC = new Process[SensorDataType,SensorDataType](parkingMCCasino)
    val parkingFT = new Process[SensorDataType,SensorDataType](parkingCCF)

    wf = wf.addActivity(parkingMC).addActivity(parkingFT)

    val urbanist = new Collector[SensorDataType]("urbanist")


    mc_set.foreach(s => wf = wf.addIO(s))
    ft_set.foreach(s => wf = wf.addIO(s))
    wf = wf.addIO(urbanist)

    val ft_l1 = new WFLink(ft_p1.output, parkingFT.getInput(ft_p1.url))
    val ft_l2 = new WFLink(ft_p2.output, parkingFT.getInput(ft_p2.url))
    val ft_l3 = new WFLink(ft_p3.output, parkingFT.getInput(ft_p3.url))
    val ft_l4 = new WFLink(ft_p4.output, parkingFT.getInput(ft_p4.url))
    val ft_l5 = new WFLink(ft_p5.output, parkingFT.getInput(ft_p5.url))
    val ft_l6 = new WFLink(ft_p6.output, parkingFT.getInput(ft_p6.url))
    val ft_l7 = new WFLink(ft_p7.output, parkingFT.getInput(ft_p7.url))
    
    val ft_links = Set(ft_l1,ft_l2,ft_l3,ft_l4,ft_l5,ft_l6,ft_l7)

    val mc_l1 = new WFLink(mc_p1.output, parkingMC.getInput(mc_p1.url))
    val mc_l2 = new WFLink(mc_p2.output, parkingMC.getInput(mc_p2.url))
    val mc_l3 = new WFLink(mc_p3.output, parkingMC.getInput(mc_p3.url))
    val mc_l4 = new WFLink(mc_p4.output, parkingMC.getInput(mc_p4.url))
    val mc_l5 = new WFLink(mc_p5.output, parkingMC.getInput(mc_p5.url))
    val mc_l6 = new WFLink(mc_p6.output, parkingMC.getInput(mc_p6.url))
    val mc_l7 = new WFLink(mc_p7.output, parkingMC.getInput(mc_p7.url))
    val mc_l8 = new WFLink(mc_p8.output, parkingMC.getInput(mc_p8.url))

    val mc_links = Set(mc_l1,mc_l2,mc_l3,mc_l4,mc_l5,mc_l6,mc_l7,mc_l8)

    val total_ft = new Sum[SensorDataType](Set("i1","i2"))

    val total_monaco = new Sum[SensorDataType](Set("i1", "i2"))

    wf = wf.addActivity(total_ft)
    wf = wf.addActivity(total_monaco)

    ft_links.foreach(l => wf = wf.addLink(l))
    mc_links.foreach(l => wf = wf.addLink(l))

    wf = wf.addLink(new WFLink(parkingFT.getOutput("ledDisplayCFF"), total_ft.getInput("i1")))
    wf = wf.addLink(new WFLink(parkingFT.getOutput("disabledParkingApplication"), total_ft.getInput("i2")))
    wf = wf.addLink(new WFLink(parkingMC.getOutput("ledDisplayMCCasino"), total_monaco.getInput("i2")))
    wf = wf.addLink(new WFLink(total_ft.output, total_monaco.getInput("i1")))

    wf = wf.addLink(new WFLink(total_monaco.output, urbanist.input))


    wf
  }



  println(ToGraphviz(monacoParking))
}
