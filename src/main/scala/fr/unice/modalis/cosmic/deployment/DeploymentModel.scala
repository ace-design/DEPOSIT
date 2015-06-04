package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{Inventory, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.core.Policy



/**
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
object Deploy {


  def prepare(policy:Policy, topology: NetworkTopology with Inventory) = {
    policy.operations.foreach(o => o.addProperty("sensors", policy.sensorsInvolved(o)))
    topology.resources.foreach(r => r.addProperty("sensors", topology.getSensorsFromNode(r)))
  }



}
