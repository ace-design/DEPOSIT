package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.PreDeploy
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 24/09/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object DemoCodeGeneration extends App{

  // Define the process
  val convert_workflow = {

    val s = GenericInput[SantanderParkingType]("parking_sensor")
    val constant = Constant(new IntegerType(1))
    val a1 = Extract[SantanderParkingType, IntegerType]("status")
    val a2 = Conditional[IntegerType]("i == 1")
    val a3a = Sub[IntegerType](Set("i1", "i2"))
    val a3b = Add[IntegerType](Set("i1", "i2"))

    val place_status = GenericOutput[IntegerType]("place_status")

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

  val s1 = EventSensor[SantanderParkingType]("s1")
  val s2 = EventSensor[SantanderParkingType]("s2")
  val c = Collector[IntegerType]("collector")

  val p1 = Process[SantanderParkingType, IntegerType](convert_workflow)
  val p2 = Process[SantanderParkingType, IntegerType](convert_workflow)

  val add = Add[IntegerType](Set("i1", "i2"))

  val l1 = Link(s1.output, p1.getInput("parking_sensor"))
  val l2 = Link(s2.output, p2.getInput("parking_sensor"))

  val l3 = Link(p1.getOutput("place_status"), add.getInput("i1"))
  val l4 = Link(p2.getOutput("place_status"), add.getInput("i2"))
  val l5 = Link(add.output, c.input)

  val policy = Policy("demo", Set(s1, s2, c), Set(p1, p2, add), Set(l1, l2, l3, l4, l5))

  val policy_expanded = PreDeploy.expandProcesses(policy)

  println(ToGraphviz(policy_expanded))
  //println(policy)

  //println(ToGraphviz(policy))


}
