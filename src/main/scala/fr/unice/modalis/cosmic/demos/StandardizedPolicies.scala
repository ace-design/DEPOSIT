package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._

/**
  * This object regroups standardized policies
  * Created by Cyril Cecchinel - I3S Laboratory on 18/11/2015.
  */
object StandardizedPolicies {

   /*
    * This policy aims at convert a raw sensor (ie. magnetic switch) to an opening detection sensor.
    * Input: input / Output: {open,closed}
    */
  val rawValueToOpeningSensor = {
    val input = GenericInput("input", classOf[SmartCampusType])
    val filtering = Conditional("value < 500", classOf[SmartCampusType])
    val open = GenericOutput("open", classOf[SmartCampusType])
    val closed = GenericOutput("closed", classOf[SmartCampusType])
    
    val l1 = Link(input.output, filtering.input)
    val l2 = Link(filtering.thenOutput, open.input)
    val l3 = Link(filtering.elseOutput, closed.input)
    
    new Policy().add(input).add(filtering).add(open).add(closed)
                .addLink(l1).addLink(l2).addLink(l3)
    
  }

   /*
    * This policy aims at convert a celsius value to a fahrenheit value
    * Input: celsius / Output: fahrenheit
    */
  val celsiusToFahrenheit = {
    val input = GenericInput("celsius", classOf[SmartCampusType])
    val multiply = Multiply(IntegerType(9),classOf[SmartCampusType])
    val divide = Divide(IntegerType(5), classOf[SmartCampusType])
    val add = Increment(IntegerType(32), classOf[SmartCampusType])
    val output = GenericOutput("fahrenheit", classOf[SmartCampusType])

    val l1 = Link(input.output, multiply.input)
    val l2 = Link(multiply.output, divide.input)
    val l3 = Link(divide.output, add.input)
    val l4 = Link(add.output, output.input)

    new Policy().add(input).add(multiply).add(divide).add(add).add(output)
                .addLink(l1).addLink(l2).addLink(l3).addLink(l4)

  }

    /*
    * This policy aims at convert a celsius value to a fahrenheit value
    * Input: fahrenheit / Output: celsius
    */
  val fahrenheitToCelsius = {
    val input = GenericInput("fahrenheit", classOf[SmartCampusType])
    val sub = Increment(IntegerType(-32), classOf[SmartCampusType])
    val multiply = Multiply(IntegerType(5), classOf[SmartCampusType])
    val divide = Divide(IntegerType(9), classOf[SmartCampusType])
    val output = GenericOutput("celsius", classOf[SmartCampusType])

    val l1 = Link(input.output, sub.input)
    val l2 = Link(sub.output, multiply.input)
    val l3 = Link(multiply.output, divide.input)
    val l4 = Link(divide.output, output.input)

    new Policy().add(input).add(sub).add(multiply).add(divide).add(output)
                .addLink(l1).addLink(l2).addLink(l3).addLink(l4)
  }
}
