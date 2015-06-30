package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.exception.NoTargetFoundException
import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{GenericNode, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.core.{DataType, Operation, Policy, Process, Sensor}

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
   * Compute the sensors involved for each operation and each node
   * @param p Data collection policy
   * @param topology Network topology
   */
  def prepare(p:Policy, topology: NetworkTopology) = {

    // Step 0: develop the processes
    val policy = expandProcesses(p)

    // Step 1: compute Sensors involved for each operation of the policy
    policy.operations.foreach(o => o.addProperty("sensors", policy.sensorsInvolved(o)))
    // Step 2: compute which sensors are reacheable from each point of the sensing infrastructure topology
    topology.resources.foreach(r => r.addProperty("sensors", topology.getSensorsFromNode(r)))

    // Step 3: compute where operations can be projected
    for (operation <- policy.operations; sensorsNeeded = operation.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[Sensor[_]]].map(_.url)) yield {
      var targets:Set[GenericNode] = Set.empty
      for (resource <- topology.resources; sensorsConnected = resource.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[fr.unice.modalis.cosmic.deployment.network.dsl.kernel.Sensor]].map(_.name)) yield {

        if (sensorsNeeded.forall(sensorsConnected contains _)) {

          // If the resource is programmable
          if (resource.isProgrammable)
            targets = targets + resource
        }
      }
      if (targets.nonEmpty)
        operation.addProperty("targets", targets)
      else throw new NoTargetFoundException(operation) //If no target has been found for an operation, the policy is undeployable
    }

    // Return the policy
    policy
  }


}

object Deploy {

  def deploy (policy:Policy, topology: NetworkTopology, targets: Map[Operation[_<:DataType, _<:DataType], GenericNode]) = {
    require(targets.keys.forall(policy.operations contains _))
    require(targets.values.forall(topology.resources contains _))


  }
}
