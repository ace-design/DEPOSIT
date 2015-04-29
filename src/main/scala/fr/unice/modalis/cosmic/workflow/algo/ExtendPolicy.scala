package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
object ExtendPolicy {


  def apply(p:Policy):Policy = {
    val toApply = (for (activity <- p.activities; if(activity.isExtendable)) yield generateJoinPointsForOperation(activity))
      .foldLeft(Set[JoinPoint[_ <:DataType]](),Set[Link[_ <:DataType]]()){(acc, e) => (acc._1 ++ e._1, acc._2 ++ e._2)}
    var policy = p
    toApply._1.foreach(j => policy = policy.add(j))
    toApply._2.foreach(l => policy = policy.addLink(l))

    policy
  }

  /**
   * Compute the join points for an operation
   * Pre-condition: The operation can be joined
   * @param o Operation
   * @tparam T Inputs type
   * @tparam O Outputs type
   * @return List of join points and List of links
   */
  def generateJoinPointsForOperation[T<:DataType, O<:DataType](o:Operation[T,O]) = {
    require(o.isExtendable)
    val linksToAdd = new ArrayBuffer[Link[_<:DataType]]()
    val iosToAdd = new ArrayBuffer[JoinPoint[_<:DataType]]()

    o.outputs.foreach(o => {
      val l = new Link(o, new JoinPointOutput(o).input)
      val s = l.destination
      linksToAdd += l
      iosToAdd += s.asInstanceOf[JoinPointOutput[_<:DataType]]
    })

    o.inputs.foreach(i => {
      val l = new Link(new JoinPointInput(i).output, i)
      val s = l.source
      linksToAdd += l
      iosToAdd += s.asInstanceOf[JoinPointInput[_<:DataType]]
    })

    (iosToAdd.toSet, linksToAdd.toSet)

  }



}
