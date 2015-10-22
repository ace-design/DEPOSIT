package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deposit.algo.{ExtendPolicy, Unification, Weave}

/**
 * Data collection policies Test (SmartSantander parking scenari)
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
object DCPTest {
  val caleHernan = Set(EventSensor("caleHernan-p1", classOf[SantanderParkingType]), EventSensor("caleHernan-p2", classOf[SantanderParkingType]), EventSensor("caleHernan-p3", classOf[SantanderParkingType]), EventSensor("caleHernan-p4", classOf[SantanderParkingType]), EventSensor("caleHernan-p5", classOf[SantanderParkingType]),
    EventSensor("caleHernan-p6-res", classOf[SantanderParkingType]),EventSensor("caleHernan-p7", classOf[SantanderParkingType]), EventSensor("caleHernan-p8", classOf[SantanderParkingType]), EventSensor("caleHernan-p9", classOf[SantanderParkingType]), EventSensor("caleHernan-p10-res", classOf[SantanderParkingType]), EventSensor("caleHernan-p11", classOf[SantanderParkingType]))

  val plaza = Set(EventSensor("plaza-p1", classOf[SantanderParkingType]), EventSensor("plaza-p2-res", classOf[SantanderParkingType]), EventSensor("plaza-p3", classOf[SantanderParkingType]))

  val caleLopeVega = Set(EventSensor("caleLopeVega-p1", classOf[SantanderParkingType]), EventSensor("caleLopeVega-p2", classOf[SantanderParkingType]), EventSensor("caleLopeVega-p3-res", classOf[SantanderParkingType]), EventSensor("caleLopeVega-p4", classOf[SantanderParkingType]))


  val convert_workflow = {

    val s = EventSensor("parking_sensor", classOf[SantanderParkingType])
    val constant = Constant(new IntegerType(1), classOf[IntegerType])
    val a1 = Extract("status", classOf[SantanderParkingType], classOf[IntegerType])
    val a2 = Conditional("i == 1", classOf[IntegerType])
    val a3a = Sub(Set("i1", "i2"), classOf[IntegerType])
    val a3b = Add(Set("i1", "i2"), classOf[IntegerType])

    val place_status = Collector("place_status", classOf[IntegerType])


    val l1 = new Link(s.output, a1.input)
    val l2 = new Link(a1.output, a2.input)
    val l3a = new Link(a2.thenOutput, a3a.getInput("i1"))
    val l3b = new Link(constant.output, a3a.getInput("i2"))

    val l4a = new Link(a2.elseOutput, a3b.getInput("i1"))
    val l4b = new Link(constant.output, a3b.getInput("i2"))

    val l5a = new Link(a3a.output, place_status.input)
    val l5b = new Link(a3b.output, place_status.input)

    new Policy("convert", Set(s,place_status), Set(a1, a2, a3a, a3b, constant), Set(l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))

  }

  val convert_workflow2 = {

    val s = EventSensor("parking_sensor", classOf[SantanderParkingType])
    val constant = Constant(new IntegerType(1), classOf[IntegerType])
    constant.setExpendable(false)
    val a1 = Extract[SantanderParkingType, IntegerType]("status", classOf[SantanderParkingType], classOf[IntegerType])
    val a2 = Conditional[IntegerType]("i == 1", classOf[IntegerType])
    val a3a = Sub[IntegerType](Set("i1", "i2"), classOf[IntegerType])
    val a3b = Add[IntegerType](Set("i1", "i2"), classOf[IntegerType])

    val place_status = Collector("place_status", classOf[IntegerType])

    val l1 = new Link(s.output, a1.input)
    val l2 = new Link(a1.output, a2.input)
    val l3a = new Link(a2.thenOutput, a3a.getInput("i1"))
    val l3b = new Link(constant.output, a3a.getInput("i2"))

    val l4a = new Link(a2.elseOutput, a3b.getInput("i1"))
    val l4b = new Link(constant.output, a3b.getInput("i2"))

    val l5a = new Link(a3a.output, place_status.input)
    val l5b = new Link(a3b.output, place_status.input)

    new Policy("convert", Set(s,place_status), Set(a1, a2, a3a, a3b, constant), Set(l1, l2, l3a, l3b, l4a, l4b, l5a, l5b))

  }

  // Application A: Count number of free places
  val collectorA = Collector("applicationA", classOf[IntegerType])

  val sensors = caleHernan ++ plaza ++ caleLopeVega
  val converters = for (x <- sensors) yield Process(convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])
  val linksToProcess = (sensors zip converters).map(x => new Link[SantanderParkingType](x._1.output, x._2.getInput("parking_sensor")))

  val adder = Add((for (i <- 1 to sensors.size) yield "i" + i).toSet, classOf[IntegerType])

  val linksToAdd = (converters zip adder.inputsNames).map(x => Link(x._1.getOutput("place_status"), adder.getInput(x._2)))

  val l = Link(adder.output, collectorA.input)



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

  val s1 = EventSensor("a", classOf[SmartCampusType])
  val a1 = Extract("v", classOf[SmartCampusType], classOf[IntegerType])
  val c1 = new Collector("collectorA", classOf[SantanderParkingType])

  val l1 = Link(s1.output, a1.input)
  val l2 = Link(a1.output, c1.input)

  val p1 = new Policy().add(s1).add(a1).add(c1).addLink(l1).addLink(l2)

  val s2 = EventSensor("b", classOf[SmartCampusType])
  val a2 = Extract("v", classOf[SmartCampusType], classOf[IntegerType])
  val c2 = new Collector("collectorB", classOf[IntegerType])

  val l12 = Link(s2.output, a2.input)
  val l22 = Link(a2.output, c2.input)

  val p2 = new Policy().add(s2).add(a2).add(c2).addLink(l12).addLink(l22)

  val add = new Add(Set("i1", "i2"), classOf[IntegerType])
  val c3 = new Collector("collectorC", classOf[IntegerType])
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
