package fr.unice.modalis.cosmic.runtime

import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deposit.core.Policy

import scala.collection.mutable
/**
  * Created by Cyril Cecchinel - I3S Laboratory on 09/02/2016.
  */
object Repository {

  val repository = mutable.Map[String, mutable.Map[String, Policy]]()

  def register(infrastructureModel: InfrastructureModel):Unit = ???

  def getPolicies(infrastructureModel: InfrastructureModel):Iterable[Policy] = ???

  def getPolicy(platform:String, infrastructureModel: InfrastructureModel):Option[Policy] = ???

  def addPolicy(policy: Policy, platform:String, infrastructureModel: InfrastructureModel):Unit = ???
}
