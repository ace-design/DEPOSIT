package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deposit.algo.{ExtendPolicy, Unification, Weave}

/**
 * Data collection policies Test (SmartSantander parking scenari)
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
object DCPTest {
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

    new Policy("convert", Set(s,place_status), Set(a1, a2, a3a, a3b, constant), Set(l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))

  }

  val convert_workflow2 = {

    val s = EventSensor[SantanderParkingType]("parking_sensor")
    val constant = Constant(new IntegerType(1))
    constant.setExpendable(false)
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

    new Policy("convert", Set(s,place_status), Set(a1, a2, a3a, a3b, constant), Set(l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))

  }

  // Application A: Count number of free places
  val collectorA = Collector[IntegerType]("applicationA")

  val sensors = caleHernan ++ plaza ++ caleLopeVega
  val converters = for (x <- sensors) yield Process[SantanderParkingType, IntegerType](convert_workflow)
  val linksToProcess = (sensors zip converters).map(x => new Link[SantanderParkingType](x._1.output, x._2.getInput("parking_sensor")))

  val adder = Add[IntegerType]((for (i <- 1 to sensors.size) yield "i" + i).toSet)

  val linksToAdd = (converters zip adder.inputsNames).map(x => Link[IntegerType](x._1.getOutput("place_status"), adder.getInput(x._2)))

  val l = Link[IntegerType](adder.output, collectorA.input)



  val dcpA = {
    var dcp = new Policy("DCPA")

    val ios = sensors ++ Set(collectorA)
    val activities: Set[Operation[_ <: DataType, _ <: DataType]] = (converters ++ List(adder)).toSet
    val links = linksToProcess ++ linksToAdd ++ List(l)

    ios.foreach(x => dcp = dcp.addIO(x))
    activities.foreach(x => dcp = dcp.addActivity(x))
    links.foreach(x => dcp = dcp.addLink(x))

    dcp
  }

  val s1 = EventSensor[SmartCampusType]("a")
  val a1 = Extract[SmartCampusType, IntegerType]("v")
  val c1 = new Collector[IntegerType]("collectorA")

  val l1 = Link(s1.output, a1.input)
  val l2 = Link(a1.output, c1.input)

  val p1 = new Policy().add(s1).add(a1).add(c1).addLink(l1).addLink(l2)

  val s2 = EventSensor[SmartCampusType]("b")
  val a2 = Extract[SmartCampusType, IntegerType]("v")
  val c2 = new Collector[IntegerType]("collectorB")

  val l12 = Link(s2.output, a2.input)
  val l22 = Link(a2.output, c2.input)

  val p2 = new Policy().add(s2).add(a2).add(c2).addLink(l12).addLink(l22)

  val add = new Add[IntegerType](Set("i1", "i2"))
  val c3 = new Collector[IntegerType]("collectorC")
  val p3 = new Policy().add(add).add(c3).addLink(Link(add.output, c3.input))


  val p1e = ExtendPolicy(p1)
  val p2e = ExtendPolicy(p2)
  val p3e = ExtendPolicy(p3)


  /** First unification **/
  val u1 = new Unification[IntegerType](
    p1e.outputJoinPoints.head.asInstanceOf[JoinPointOutput[IntegerType]],
    p3e.inputJoinPoints.find(p => p.toConceptInput.name == add.getInput("i1").name).get.asInstanceOf[JoinPointInput[IntegerType]])

  val res = Weave(p1e, p3e, Set(u1))
  val rese = ExtendPolicy(res)
  /** Second unification **/
  val u2 = new Unification[IntegerType](
    p2e.outputJoinPoints.head.asInstanceOf[JoinPointOutput[IntegerType]],
    rese.inputJoinPoints.find(p => p.toConceptInput.name == add.getInput("i2").name).get.asInstanceOf[JoinPointInput[IntegerType]])


  val resfinal = Weave(rese, p2e, Set(u2))


}
