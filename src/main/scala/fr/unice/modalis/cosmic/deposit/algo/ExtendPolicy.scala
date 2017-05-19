package fr.unice.modalis.cosmic.deposit.algo

import fr.unice.modalis.cosmic.deposit.core._

/**
 * Methods and objects related to (de)composition operators
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
case class Unification[T<:DataType](a:JoinPointOutput[T], b:JoinPointInput[T])

object ExtendPolicy {

  def apply(p: Policy, onlyEmptyPorts: Boolean = false): Policy = {
    var newPolicy = p
    p.concepts.foreach {
      case c: Operation[_,_] =>
        c.inputs.foreach { input =>
          if (!p.flows.exists(_.destination_input == input)) {
            val jpi = new JoinPointInput(input, c.iType); newPolicy = newPolicy.add(jpi); newPolicy = newPolicy.add(new Flow(jpi.output, input))
          }
        }
        c.outputs.foreach { output =>
          if (!onlyEmptyPorts || (onlyEmptyPorts && !p.flows.exists(_.source_output == output))) {
            val jpo = new JoinPointOutput(output, c.oType)
            newPolicy = newPolicy.add(jpo);
            newPolicy = newPolicy.add(new Flow(output, jpo.input))
          }
        }
      case i: DataInput[_] => if (!onlyEmptyPorts || (onlyEmptyPorts && !p.flows.exists(_.source_output == i.output))) {
        val jpo = new JoinPointOutput(i.output, i.dataType); newPolicy = newPolicy.add(jpo); newPolicy = newPolicy.add(new Flow(i.output, jpo.input))
      }
      case o: DataOutput[_] => if (!p.flows.exists(_.destination == o)) {
        val jpi = new JoinPointInput(o.input, o.dataType); newPolicy = newPolicy.add(jpi); newPolicy = newPolicy.add(new Flow(jpi.output, o.input))
      }
    }

    newPolicy
  }
}

object FactorizePolicy {

  def apply(p:Policy) = deleteJoinPointsForOperation(p)

  /**
   * Remove all join points in a policy
    *
    * @param p Input policy
   * @return A new policy without join points
   */
  def deleteJoinPointsForOperation(p:Policy) = {
    var policy = p
    p.ios.collect( {case x:JoinPoint[_] => x}).foreach(e => policy = policy.deleteIO(e))
    policy
  }
}


object Weave {

  def apply(p1: Policy, p2: Policy, associations:Set[Unification[_<:DataType]]) = {
    val flows = for (x<-associations) yield createFlow(x)
    var newPolicy = p1 + p2
    for (l <- flows) yield newPolicy = newPolicy.addFlow(l)
    FactorizePolicy(newPolicy)
  }

  def createFlow[T<:DataType](u:Unification[T]) = {
    new Flow[T](u.a.fromConceptOutput, u.b.toConceptInput)
  }

}