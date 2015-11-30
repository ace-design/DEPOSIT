package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.exception.NoTargetFoundException
import fr.unice.modalis.cosmic.deployment.heuristics.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{GenericNode, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.algo.ExtendPolicy
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._

import scala.collection.mutable

/**
 * Prepare the deployment of a data collection policy
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
object PreDeploy {

  def apply(policy: Policy, topology: NetworkTopology) = prepare(policy, topology)

  /**
   * Expand processes
   * @param p Policy
   * @return A policy with processes expanded
   */
  def expandProcesses(p:Policy) = {
    var policy = p
    p.operations collect {case x:Process[_,_] => x } foreach {process => policy = process.expand(policy)}
    policy
  }

  /**
   * Get the repartition of operation on a given topology
   * @param p Policy
   * @param topology Network topology
   * @return A hashmap (GenericNode, Set(operations))
   */
  def getOperationRepartition(p:Policy, topology: NetworkTopology) = {
    val repartition = mutable.HashMap[GenericNode, Set[Operation[_<:DataType, _<:DataType]]]()
    p.operations.map(o => o.readProperty("targets"))
    for (resource <- topology.resources if resource.isProgrammable) yield {
      val selection = p.operations.filter(o => o.readProperty("targets").getOrElse(false).asInstanceOf[Set[GenericNode]] contains resource)
      repartition += ((resource, selection))
    }
    repartition
  }

  /**
   * Compute the sensors involved for each operation and each node
   * @param p Data collection policy
   * @param topology Network topology
   */
  def prepare(p:Policy, topology: NetworkTopology) = {

    // Step 0: develop the processes
    val policy = expandProcesses(p)

    // Step 1: compute Sensors involved for each operation of the policy
    policy.concepts.foreach(c => c.addProperty("sensors", policy.sensorsInvolved(c)))
    // Step 2: compute which sensors are reachable from each point of the sensing infrastructure topology
    topology.resources.foreach(r => r.addProperty("sensors", topology.getSensorsFromNode(r)))

    // Step 3: compute where operations can be projected
    for (concept <- policy.concepts; sensorsNeeded = concept.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[Sensor[_]]].map(_.url)) yield {
      var targets:Set[GenericNode] = Set.empty
      for (resource <- topology.resources; sensorsConnected = resource.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[fr.unice.modalis.cosmic.deployment.network.dsl.kernel.Sensor]].map(_.name)) yield {

        if (sensorsNeeded.forall(sensorsConnected.contains)) {

          // If the resource is programmable
          if (resource.isProgrammable)
            targets = targets + resource
        }
      }
      if (targets.nonEmpty)
        concept.addProperty("targets", targets)
      else throw new NoTargetFoundException(concept) //If no target has been found for an concept, the policy can not be deployed
    }

    // Return the policy
    policy
  }


}

object Deploy {


  def apply(policy: Policy, topology: NetworkTopology, targets: Map[Concept, String]) = deploy(policy, topology, targets)
  def apply(policy: Policy, topology: NetworkTopology, heuristic: DeploymentRepartition) = deploy(policy, topology, heuristic)


  private def getNetworkLinks(policies:Set[Policy], ref:Policy) = {
    ref.links -- policies.foldLeft(new Policy()){(acc, e) => acc ++ e}.links
  }


  /**
    * Manual deployment of a pre-deployed policy over a sensing infrastructure
    * @param policy Pre-deployed policy
    * @param topology Network topology model
    * @param targets Manual association concept -> platform
    * @return A policy for each platform of the sensing infrastructure
    */
  def deploy (policy:Policy, topology: NetworkTopology, targets: Map[Concept, String]):Iterable[Policy] = {
    /**
      * Delete join points not involved in a network communication
      * @param policy Policy
      * @return A policy without join points not involved in a network communication
      */
    def deleteNonRevelantJoinPoints(policy:Policy) = {

      val joinPoints = policy.ios.toList.collect{case x:JoinPoint[_] => x}.filterNot(_.hasProperty("network").isDefined)
      var sanitizedPolicy = policy
      joinPoints.foreach(c => sanitizedPolicy = sanitizedPolicy.delete(c))
      sanitizedPolicy
    }

    // Associating a concept to a platform
    val projection = targets.map(t => (t._1, topology.resources.find(r => r.name equals t._2 ).get))
    val projectionGrouped = projection.toSeq.groupBy(_._2).map { e => e._1 -> e._2.map(_._1)}

    // Split the global policies into sub-policy and extend it with Join Points
    val rawPolicies = projectionGrouped.map(e => ExtendPolicy(policy.select(e._2.toSet, policy.name + "_" + e._1.name),emptyIOonly = false))

    // Compute which join points are network-related
    val networkLinks = getNetworkLinks(rawPolicies.toSet, policy)


    // Associating same network id to linked join points
    for (l <- networkLinks.groupBy(_.source_output); src = l._1; dsts = l._2.map(_.destination_input)) {

      val uid = scala.util.Random.alphanumeric.take(5).mkString
      val optionJoinPointOutput = rawPolicies.find(aPolicy => aPolicy.concepts contains src.parent).get.nextElements(src.parent).collect {case (x:JoinPointOutput[_], _) => x}.find(_.fromConceptOutput == src)
      if (optionJoinPointOutput.isDefined) optionJoinPointOutput.get.addProperty("network",uid)
      for (dst <- dsts) {
        val optionJoinPointInput = rawPolicies.find(aPolicy => aPolicy.concepts contains dst.parent).get.previousElements(dst.parent).collect {case (x:JoinPointInput[_], _) => x}.find(_.toConceptInput == dst)
        if (optionJoinPointInput.isDefined) optionJoinPointInput.get.addProperty("network",uid)
      }
    }

    // Delete non-relevant join points
    val readyToDeployedPolicies = rawPolicies.map { deleteNonRevelantJoinPoints }
    readyToDeployedPolicies.foreach(p => ToGraphviz.writeSource(p))

    readyToDeployedPolicies

  }

  /**
    * Deploy a policy over a sensing infrastructure according a heuristic
    * @param policy Pre-deployed policy
    * @param topology Network topology model
    * @param heuristic Manual association concept -> platform
    * @return A policy for each platform of the sensing infrastructure
    */
  def deploy(policy: Policy, topology: NetworkTopology, heuristic: DeploymentRepartition):Iterable[Policy] = {
    val targets = policy.concepts.map { c =>
      val place = heuristic.place(c, topology)
      c -> place.name
    }.toMap[Concept, String]

    deploy(policy, topology, targets)
  }
}
