package fr.unice.modalis.cosmic.runtime

import com.typesafe.scalalogging.LazyLogging
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.network.Entity
import fr.unice.modalis.cosmic.deposit.core.Policy

import scala.collection.mutable
/**
  * Created by Cyril Cecchinel - I3S Laboratory on 09/02/2016.
  */

/**
  * This singleton object represents a Repositories Manager
  */
object RepositoriesManager {

  /**
    * Add a repository to the manager
    * @param repo Repository
    * @param name Repository's name (default: Network topology's name)
    */
  def addRepository(repo:Repository, name:Option[String] = None):Unit = {
    if (name.isDefined)
      repositories(name.get) = repo
    else repositories(repo.infrastructureModel.topology.name) = repo
  }

  /**
    * Find a repository by its name
    * @param name Repository's name
    * @return An option value containing the lookup result
    */
  def getRepository(name:String):Option[Repository] = repositories.get(name)

  /**
    * Remove a repository by its name
    * @param name Repository's name
    */
  def removeRepository(name:String):Unit = repositories -= name

  private val repositories = mutable.Map[String, Repository]()

}

/**
  * Repository
  * @param infrastructureModel Infrastructure Model
  */
case class Repository(infrastructureModel: InfrastructureModel) extends LazyLogging{

  /**
    * Get the association between entities and policies
    * @return a map between entities and policies
    */
  def getPolicies = policies.toMap

  /**
    * Update a policy on an entity by its name
    * @param policy Policy
    * @param entityName Entity's name
    */
  def updatePolicy(policy: Policy, entityName: String):Unit = {
    assert(infrastructureModel.topology.resources.exists(_.name equals entityName))
    updatePolicy(policy, infrastructureModel.topology.resources.find(_.name equals entityName).get)
  }

  /**
    * Update a policy on an entity
    * @param policy Policy
    * @param entity Entity
    */
  def updatePolicy(policy: Policy, entity: Entity):Unit = {
    assert(infrastructureModel.topology.resources.contains(entity))
    logger.debug("Update entity " + entity.name + " with policy " + policy.name)
    if (policies.contains(entity))
      logger.warn("Replacing " + policies(entity).name + " with " + policy.name + " on " + entity.name)
    policies.update(entity, policy)
  }

  /**
    * Get the last policy deployed on an entity (by its name)
    * @param entityName Entity name
    * @return Last policy deployed on an entity
    */
  def getPolicy(entityName: String):Option[Policy] = {
    assert(infrastructureModel.topology.resources.exists(_.name equals entityName))
    getPolicy(infrastructureModel.topology.resources.find(_.name equals entityName).get)
  }

  /**
    * Get the last policy deployed on an entity
    * @param entity Entity
    * @return Last policy deployed on an entity
    */
  def getPolicy(entity: Entity):Option[Policy] = {
    assert(infrastructureModel.topology.resources.contains(entity))
    policies.get(entity)
  }

  private val policies = mutable.Map[Entity, Policy]()

}

case class RepositoryNotFoundException(name:String) extends Exception(s"The repository $name does not exist")