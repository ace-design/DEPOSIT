package fr.unice.modalis.cosmic.deposit.algo

import fr.unice.modalis.cosmic.deposit.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Methods and objects related to (de)composition operators
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
case class Unification[T<:DataType](a:JoinPointOutput[T], b:JoinPointInput[T])

object ExtendPolicy {


  def generateJoinPointsForInterface(interface: PolicyIO[_ <: DataType], p: Policy, onlyEmptyPorts: Boolean) = {

    interface match {
      case i:Sensor[_] =>
        if (onlyEmptyPorts && p.flowsFrom(i).nonEmpty) {
          (None,None)
        }
        else {
          val jp = new JoinPointOutput(i.output, i.dataType)
          val l = new Flow(i.output, jp.input)
          (Some(jp),Some(l))
        }

      case i:Collector[_] =>
        if (p.flowsTo(i).nonEmpty) (None, None)
        else {
          val jp = new JoinPointInput(i.input, i.dataType)
          val l = new Flow(jp.output, i.input)
          (Some(jp), Some(l))
        }
      }

  }

  def apply(p:Policy, onlyEmptyPorts:Boolean = false):Policy = {

    val interfacesExtended = for (interface <- p.ios -- p.ios.collect {case x:JoinPoint[_] => x}) yield generateJoinPointsForInterface(interface, p, onlyEmptyPorts)

    val toApply = (for (activity <- p.operations; if activity.isExpendable) yield generateJoinPointsForOperation(activity, p, onlyEmptyPorts))
      .foldLeft(Set[JoinPoint[_ <:DataType]](),Set[Flow[_ <:DataType]]()){ (acc, e) => (acc._1 ++ e._1, acc._2 ++ e._2)}
    var policy = p
    toApply._1.foreach(j => policy = policy.add(j))
    toApply._2.foreach(l => policy = policy.addFlow(l))

    for (i <- interfacesExtended) {
      if (i._1.isDefined) policy = policy.add(i._1.get)
      if (i._2.isDefined) policy = policy.addFlow(i._2.get)
    }

    policy
  }



  /**
   * Compute the join points for an operation
   * Pre-condition: The operation can be joined
   * @param op Operation
   * @tparam I Inputs type
   * @tparam O Outputs type
   * @return List of join points and List of links
   */
  def generateJoinPointsForOperation[I<:DataType, O<:DataType](op:Operation[I,O], p:Policy, emptyIOonly:Boolean = false) = {
    require(op.isExpendable)
    require(p.operations contains op)
    var outputs:Set[Output[O]] = Set()
    var inputs:Set[Input[I]] = Set()
    val flowsToAdd = new ArrayBuffer[Flow[_<:DataType]]()
    val iosToAdd = new ArrayBuffer[JoinPoint[_<:DataType]]()



    inputs =  op.inputs -- p.flowsTo(op).map { l => l.destination_input}.asInstanceOf[Set[Input[I]]]
    if (emptyIOonly)
      outputs =  op.outputs -- p.flowsFrom(op).map { l => l.source_output}.asInstanceOf[Set[Output[O]]]
    else
      outputs = op.outputs

    outputs.foreach(o => {
      val l = new Flow(o, new JoinPointOutput(o, op.oType).input)
      val s = l.destination
      flowsToAdd += l
      iosToAdd += s.asInstanceOf[JoinPointOutput[_ <: DataType]]
    })

    inputs.foreach(i => {
        val l = new Flow(new JoinPointInput(i, op.iType).output, i)
        val s = l.source
        flowsToAdd += l
        iosToAdd += s.asInstanceOf[JoinPointInput[_<:DataType]]
    })

    (iosToAdd.toSet, flowsToAdd.toSet)

  }

}

object FactorizePolicy {

  def apply(p:Policy) = deleteJoinPointsForOperation(p)

  /**
   * Remove all join points in a policy
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
    var newPolicy = p1 ++ p2
    for (l <- flows) yield newPolicy = newPolicy.addFlow(l)
    FactorizePolicy(newPolicy)
  }

  def createFlow[T<:DataType](u:Unification[T]) = {
    new Flow[T](u.a.fromConceptOutput, u.b.toConceptInput)
  }

}