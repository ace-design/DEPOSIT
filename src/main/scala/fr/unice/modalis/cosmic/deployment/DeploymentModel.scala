package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{GenericNode, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.core.{DataType, Operation, Policy, Sensor}



/**
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
object PreDeploy {


  def apply(policy: Policy, topology: NetworkTopology) = prepare(policy, topology)

  def prepare(policy:Policy, topology: NetworkTopology ) = {


    policy.operations.foreach(o => o.addProperty("sensors", policy.sensorsInvolved(o)))
    topology.resources.foreach(r => r.addProperty("sensors", topology.getSensorsFromNode(r)))

    for (o <- policy.operations; p = o.readProperty("sensors").asInstanceOf[Set[Sensor[_]]].map(_.url)) yield {
      var targets:Set[GenericNode] = Set.empty
      for (o2 <- topology.resources; p2 = o2.readProperty("sensors").asInstanceOf[Set[fr.unice.modalis.cosmic.deployment.network.dsl.kernel.Sensor]].map(_.name)) yield {

        if (p.forall(p2 contains _)) {
          targets = targets + o2
        }
      }
      o.addProperty("targets", targets)
    }
  }


}

object Deploy {

  def deploy (policy:Policy, topology: NetworkTopology, targets: Map[Operation[_<:DataType, _<:DataType], GenericNode]) = {
    require(targets.keys.forall(policy.operations contains _))
    require(targets.values.forall(topology.resources contains _))


  }
}
