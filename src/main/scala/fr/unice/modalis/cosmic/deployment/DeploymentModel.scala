package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.exception.NoTargetFoundException
import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{GenericNode, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.core.{DataType, Operation, Policy, Sensor}

/**
 * Prepare the deployment of a data collection policy
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
object PreDeploy {


  def apply(policy: Policy, topology: NetworkTopology) = prepare(policy, topology)

  /**
   * Compute the sensors involved for each operation and each node
   * @param policy Data collection policy
   * @param topology Network topology
   */
  def prepare(policy:Policy, topology: NetworkTopology) = {


    policy.operations.foreach(o => o.addProperty("sensors", policy.sensorsInvolved(o)))
    topology.resources.foreach(r => r.addProperty("sensors", topology.getSensorsFromNode(r)))

    for (operation <- policy.operations; sensorsNeeded = operation.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[Sensor[_]]].map(_.url)) yield {
      var targets:Set[GenericNode] = Set.empty
      for (resource <- topology.resources; sensorsConnected = resource.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[fr.unice.modalis.cosmic.deployment.network.dsl.kernel.Sensor]].map(_.name)) yield {

        if (sensorsNeeded.forall(sensorsConnected contains _)) {

          if (resource.isProgrammable)
            targets = targets + resource
        }
      }
      if (targets.nonEmpty)
        operation.addProperty("targets", targets)
      else throw new NoTargetFoundException(operation)
    }
  }


}

object Deploy {

  def deploy (policy:Policy, topology: NetworkTopology, targets: Map[Operation[_<:DataType, _<:DataType], GenericNode]) = {
    require(targets.keys.forall(policy.operations contains _))
    require(targets.values.forall(topology.resources contains _))


  }
}
