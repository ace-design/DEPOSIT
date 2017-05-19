package fr.unice.modalis.cosmic.runtime

import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.network.Entity
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core.Policy



/**
  * Created by Cyril Cecchinel - I3S Laboratory on 10/05/2017.
  */
class InfrastructureManager(infrastructure:InfrastructureModel) {

  private var currentPolicy:Policy = _
  private var policiesExecuted:List[Policy] = List()
  private var subPolicies:Map[Entity, Policy] = Map()

  def registerPolicy(policy: Policy):Unit = {
    policiesExecuted = policy :: policiesExecuted
    currentPolicy = policiesExecuted.reduceLeft(_ + _)

    val preDeployed = PreDeploy(currentPolicy, infrastructure.topology)
    subPolicies = Map()
    Deploy(preDeployed, infrastructure.topology, infrastructure.strategy).foreach{ policy =>
      subPolicies = subPolicies + ((infrastructure.topology.findEntityByName(policy.target()).get, policy))
      //policy.generate()
    }
  }

  def getPolicy(e:Entity):Policy = subPolicies(e)

  def getPolicy():Policy = currentPolicy

  def stats() = {
    println("**** Statistics : ****")
    println(s"#Deployed policices: ${policiesExecuted.size}")
    println("Global Policy:")
    println(s"\t# Activities:${currentPolicy.concepts.size}")
    println(s"\t# Flows:${currentPolicy.flows.size}")
    println()
    println(s"#Subpolicies:${subPolicies.size}")
    subPolicies.foreach{sub => println(s"\tPlatform ${sub._1.name}")
    println(s"\t\t# Activities:${sub._2.concepts.size}")
      println(s"\t\t# Flows:${sub._2.flows.size}")}

  }

}
