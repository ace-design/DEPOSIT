package fr.unice.modalis.cosmic.deposit.scenarios

import fr.unice.modalis.cosmic.deployment.PreDeploy
import fr.unice.modalis.cosmic.deployment.generator.ArduinoGenerator
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 24/09/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object DemoCodeGeneration extends App{

  // Define the process
  val convert_workflow = {

    val s = GenericInput("parking_sensor", classOf[SantanderParkingType])
    val a1 = Extract("status", classOf[SantanderParkingType], classOf[IntegerType])
    val a2 = Conditional("i == 1", classOf[IntegerType])

    val a3a = Increment(IntegerType(-1), classOf[IntegerType])
    val a3b = Increment(IntegerType(+1), classOf[IntegerType])

    val place_status = GenericOutput("place_status", classOf[IntegerType])

    val l1 = new Link(s.output, a1.input)
    val l2 = new Link(a1.output, a2.input)

    val l3a = new Link(a2.thenOutput, a3a.input)


    val l4a = new Link(a2.elseOutput, a3b.input)


    val l5a = new Link(a3a.output, place_status.input)
    val l5b = new Link(a3b.output, place_status.input)

    new Policy("convert", Set(s,place_status), Set(a1, a2, a3a, a3b), Set(l1, l2, l3a, l4a, l5a, l5b))

  }

  val s1 = EventSensor("TEST", classOf[SantanderParkingType])
  val s2 = EventSensor("s2", classOf[SantanderParkingType])
  val c = Collector("collector", classOf[IntegerType])

  val p1 = Process(convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])
  val p2 = Process(convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])

  val add = Add(Set("i1", "i2"), classOf[IntegerType])

  val l1 = Link(s1.output, p1.getInput("parking_sensor"))
  val l2 = Link(s2.output, p2.getInput("parking_sensor"))

  val l3 = Link(p1.getOutput("place_status"), add.getInput("i1"))
  val l4 = Link(p2.getOutput("place_status"), add.getInput("i2"))
  val l5 = Link(add.output, c.input)

  val policy = Policy("demo", Set(s1, s2, c), Set(p1, p2, add), Set(l1, l2, l3, l4, l5))


  val policy_expanded = PreDeploy.expandProcesses(policy)

  //println(ToGraphviz(policy_expanded))
  //println(ArduinoGenerator.orderedGenerationList(policy_expanded))

println(ArduinoGenerator(policy_expanded))
//  val res = ArduinoGenerator.generateInstructionList(policy_expanded).flatMap(i => Set(i.inputs, i.outputs)).flatten

  //println(ArduinoGenerator.generateVariablesDeclaration(res.toSet))
  /*println(ArduinoGenerator.generateUpdateMethod(policy_expanded))
  println(ArduinoGenerator.generateInstructionList(policy_expanded).foldLeft(""){(acc, e) => acc + e.body + "\n"})*/

}
