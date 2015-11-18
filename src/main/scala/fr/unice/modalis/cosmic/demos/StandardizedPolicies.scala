package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._

/**
  * This object regroups standardized policies
  * Created by Cyril Cecchinel - I3S Laboratory on 18/11/2015.
  */
object StandardizedPolicies {

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
}
