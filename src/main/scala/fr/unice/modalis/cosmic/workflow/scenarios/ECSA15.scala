package fr.unice.modalis.cosmic.workflow.scenarios

import fr.unice.modalis.cosmic.workflow.converter.ToGraphviz
import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 15/04/15.
 */
object ECSA15 extends App{

  // City of Santander - Smart Parking sensors


  val caleHernan = Set(EventSensor[SantanderParkingType]("caleHernan-p1"), EventSensor[SantanderParkingType]("caleHernan-p2"), EventSensor[SantanderParkingType]("caleHernan-p3"), EventSensor[SantanderParkingType]("caleHernan-p4"), EventSensor[SantanderParkingType]("caleHernan-p5"),
    EventSensor[SantanderParkingType]("caleHernan-p6-res"),EventSensor[SantanderParkingType]("caleHernan-p7"), EventSensor[SantanderParkingType]("caleHernan-p8"), EventSensor[SantanderParkingType]("caleHernan-p9"), EventSensor[SantanderParkingType]("caleHernan-p10-res"), EventSensor[SantanderParkingType]("caleHernan-p11"))

  val plaza = Set(EventSensor[SantanderParkingType]("plaza-p1"), EventSensor[SantanderParkingType]("plaza-p2-res"), EventSensor[SantanderParkingType]("plaza-p3"))

  val caleLopeVega = Set(EventSensor[SantanderParkingType]("caleLopeVega-p1"), EventSensor[SantanderParkingType]("caleLopeVega-p2"), EventSensor[SantanderParkingType]("caleLopeVega-p3-res"), EventSensor[SantanderParkingType]("caleLopeVega-p4"))


  val convert_workflow = {

    val s = EventSensor[SantanderParkingType]("parking_sensor")
    val constant = Constant(new IntegerType(1))
    val a1 = Extract[SantanderParkingType, IntegerType]("status")
    val a2 = Conditional[IntegerType]("i == 1")
    val a3a = Sub[IntegerType](Set("i1", "i2"))
    val a3b = Add[IntegerType](Set("i1", "i2"))

    val place_status = Collector[IntegerType]("place_status")

    val l1 = new Link[SantanderParkingType](s.output, a1.input)
    val l2 = new Link[IntegerType](a1.output, a2.input)
    val l3a = new Link[IntegerType](a2.thenOutput, a3a.getInput("i1"))
    val l3b = new Link[IntegerType](constant.output, a3a.getInput("i2"))

    val l4a = new Link[IntegerType](a2.elseOutput, a3b.getInput("i1"))
    val l4b = new Link[IntegerType](constant.output, a3b.getInput("i2"))

    val l5a = new Link[IntegerType](a3a.output, place_status.input)
    val l5b = new Link[IntegerType](a3b.output, place_status.input)

    new Workflow("convert", Set(s,place_status), Set(a1, a2, a3a, a3b, constant), Set(l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))

  }

  // Application A: Count number of free places
  val dcpA = {
    val collectorA = Collector[IntegerType]("applicationA")

    val sensors = caleHernan ++ plaza ++ caleLopeVega
    val converters = for (x <- sensors) yield Process[SantanderParkingType, IntegerType](convert_workflow)
    val linksToProcess = (sensors zip converters).map(x => new Link[SantanderParkingType](x._1.output, x._2.getInput("parking_sensor")))

    val adder = Add[IntegerType]((for (i <- 1 to sensors.size) yield "i" + i).toSet)

    val linksToAdd = (converters zip adder.inputsNames).map(x => Link[IntegerType](x._1.getOutput("place_status"), adder.getInput(x._2)))

    val l = Link[IntegerType](adder.output, collectorA.input)

    var dcp = new Workflow("DCPA")

    val ios = sensors ++ Set(collectorA)
    val activities: Set[Operation[_ <: DataType, _ <: DataType]] = (converters ++ List(adder)).toSet
    val links = (linksToProcess ++ linksToAdd ++ List(l))

    ios.foreach(x => dcp = dcp.addIO(x))
    activities.foreach(x => dcp = dcp.addActivity(x))
    links.foreach(x => dcp = dcp.addLink(x))

    dcp
  }

  val dcpD = {
    // Application D: Monitor illegal parking

    val sensors = (caleHernan ++ plaza ++ caleLopeVega).filter(_.output.name.contains("res"))

    val collectorD = Collector[SantanderParkingType]("applicationD")

    val a = Conditional[SantanderParkingType]("i.status == 1")
    val linksToConditional = sensors.map(x => new Link[SantanderParkingType](x.output, a.input))
    val l = Link[SantanderParkingType](a.thenOutput, collectorD.input)

    var dcp = new Workflow("DCPD")
    val ios = sensors ++ Set(collectorD)
    val activities = Set(a)
    val links = (linksToConditional ++ List(l))

    ios.foreach(x => dcp = dcp.addIO(x))
    activities.foreach(x => dcp = dcp.addActivity(x))
    links.foreach(x => dcp = dcp.addLink(x))

    dcp

  }


  //println(dcpD.ios)
  println(ToGraphviz(convert_workflow))


}
