package fr.unice.modalis.cosmic.deployment

import java.util.NoSuchElementException

import com.typesafe.scalalogging.LazyLogging
import fr.unice.modalis.cosmic.deployment.exception.NoTargetFoundException
import fr.unice.modalis.cosmic.deployment.generator.CodeGenerator
import fr.unice.modalis.cosmic.deployment.infrastructure.{Features, InfrastructureModel, NetworkTopology}
import fr.unice.modalis.cosmic.deployment.network.{Entity, GenericNode}
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deposit.algo.ExtendPolicy
import fr.unice.modalis.cosmic.deposit.converter.ToGraphviz
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.runtime.{RepositoriesManager, RepositoryNotFoundException}

import scala.collection.mutable

object AutoDeploy extends LazyLogging {

  def apply(policy: Policy, infrastructureModel: InfrastructureModel, customRepository:Option[String] = None):Unit = {
    logger.info("AutoDeploy initialisation of " + policy.name + " on " + infrastructureModel.topology.name)
    val repository = RepositoriesManager.getRepository(customRepository.getOrElse(infrastructureModel.topology.name)).getOrElse {
      logger.error("Repository not found")
      throw new RepositoryNotFoundException(infrastructureModel.topology.name)
    }

    val tbegin = System.currentTimeMillis()
    val policies = Decompose(policy, infrastructureModel)
    println(s"Decomposition (${policy.name}): ${System.currentTimeMillis() - tbegin} ms")


    logger.debug("Registering policies in repository...")
    policies.foreach { p =>
      val target = p.readProperty("board").get.asInstanceOf[String]
      val newPolicy = repository.getPolicy(target).getOrElse(new Policy("")) + p //If no policy was found, compose with empty policy

      // Recopy only relevant properties in repository
      newPolicy.addProperty("generator", p.readProperty("generator").get)
      newPolicy.addProperty("board", p.readProperty("board").get)

      repository.updatePolicy(newPolicy, target)
    }

    logger.debug("Generating source file...")
    repository.getPolicies.values.foreach { p =>
      logger.debug("Generate source file for " + p.name)
      val generator = p.readProperty("generator").get.asInstanceOf[CodeGenerator]
      logger.debug("Calling " + generator + "for " + p.name)
      generator.apply(p, toFile = true)
    }



  }
}
/**
  * Auto-decomposition of data collection policy
  */
object Decompose extends LazyLogging{
  /**
    * Decompose a data collection policy
    *
    * @param policy Data collection policy
    * @param infrastructureModel Infrastructure model
    * @return A policy for each platform of the sensing infrastructure
    */
  def apply(policy: Policy, infrastructureModel: InfrastructureModel):Iterable[Policy] ={
    logger.info("Ready to deploy " + policy.name + " on " + infrastructureModel.topology.name + " with " + infrastructureModel.strategy.toString)
    Deploy(PreDeploy(policy, infrastructureModel.topology), infrastructureModel.topology, infrastructureModel.strategy)
  }
}

/**
 * Prepare the deployment of a data collection policy
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
object PreDeploy extends LazyLogging{

  def apply(policy: Policy, topology: NetworkTopology) = {
    val tbegin = System.currentTimeMillis()
    val res = prepare(policy, topology)
    println(s"\tTime PreDeploy: ${System.currentTimeMillis() - tbegin} ms")
    res
  }

  def orderedGeneration(policy: Policy, topology: NetworkTopology) = {
    val activitiesAndPlatforms = CodeGenerator.orderedGenerationList(policy).map{e => (e, e.readProperty("targets").get.asInstanceOf[Set[Entity]])}
    val orderedTopology = topology.orderedTopology

    def filterPlatforms(l:List[(Concept, Set[Entity])]):Unit = {
      l match {
        case a :: b :: tail =>
          val result = a._2.filterNot(e => {
            val indexA = orderedTopology.indexOf(e)
            val indexesB = b._2.map {
              orderedTopology.indexOf(_)
            }
            indexesB.map { s => s < indexA }.nonEmpty
          }

          )
          a._1.addProperty("concepts", result)
          filterPlatforms(b :: tail)
        case a :: Nil => ()
        case Nil => ()
      }
    }

    filterPlatforms(activitiesAndPlatforms)
  }

  /**
   * Expand processes
    *
    * @param p Policy
   * @return A policy with processes expanded
   */
  def expandProcesses(p:Policy) = {
    var policy = p
    p.operations collect {case x:Process[_,_] => x } foreach {process => policy = process.expand(policy)}
    policy.operations.filter(o => !policy.flows.exists(p => p.source == o)).foreach(o => policy = policy.delete(o))
    policy
  }

  /**
   * Get the repartition of operation on a given topology
    *
    * @param p Policy
   * @param topology Network topology
   * @return A hashmap (GenericNode, Set(operations))
   */
  def getOperationRepartition(p:Policy, topology: NetworkTopology) = {
    val repartition = mutable.HashMap[GenericNode, Set[Operation[_<:DataType, _<:DataType]]]()
    for (resource <- topology.resources if resource.isProgrammable) yield {
      val selection = p.operations.filter(o => o.readProperty("targets").getOrElse(false).asInstanceOf[Set[GenericNode]] contains resource)
      repartition += ((resource, selection))
    }
    repartition
  }

  /**
   * Compute the sensors involved for each operation and each node
    *
    * @param p Data collection policy
   * @param topology Network topology
   */
  def prepare(p:Policy, topology: NetworkTopology) = {

    // Step 0: develop the processes
    var policy:Policy = p
    while (policy.operations.collect({case x:Process[_,_] => x}).nonEmpty)
      policy = expandProcesses(policy)

    // Step 0* : Refine policy sensors with Type and Brand information from topology model
    policy.sources.collect {case x:Sensor[_] => x}.foreach { s =>
      val sensor = topology.findSensorByName(s.name)
      try {
        s.addProperty("type", sensor.get.sType)
        s.addProperty("brand", sensor.get.sBrand)
        if (sensor.get.sPin.isDefined)
          s.addProperty("pin", sensor.get.sPin.get)
      } catch {
        case e:NoSuchElementException => logger.warn("Sensor " + s.name + " has not been found in " + topology.name)
      }
    }

    // Step 0** : Refine collectors with public urls and connection type from topology model
    policy.collectors.foreach {c =>
      val collector = topology.findEntityByName(c.name)
      try{
        if (collector.get.publicUrl.isDefined) {
          c.addProperty("url", collector.get.publicUrl.get)
        }
        } catch {
        case e:NoSuchElementException => logger.warn("Collector " + c.name + " has not been found in " + topology.name)
      }
      try {
        val edge = topology.edges.find(_.destination equals c.name).get
        c.addProperty("connection", edge.media)

      } catch {
        case e:NoSuchElementException => logger.warn("No media type found in " + topology.name + " for " + c.name)
      }

      }


    // Step 1: compute Sensors involved for each operation of the policy
    policy.concepts.foreach(c => c.addProperty("sensors", policy.sensorsInvolved(c)))

    // Step 2: compute which sensors are reachable from each point of the sensing infrastructure topology
    // Moved to topological model

    // Step 3: compute where operations can be projected
    for (concept <- policy.concepts; sensorsNeeded = concept.readProperty("sensors").getOrElse(Set[Sensor[_]]()).asInstanceOf[Set[Sensor[_]]].map(_.url)) yield {
      logger.debug(s"$concept needs $sensorsNeeded")
      var targets:Set[GenericNode] = Set.empty
      for (resource <- topology.resources; sensorsConnected = topology.reachableSensors(resource.name).map{_.name}) yield {
        if (sensorsNeeded.forall(sensorsConnected.contains)) {
          // If the resource is programmable and the concept can be projected on the resource
          logger.debug(s"${resource.name} reaches all the sensors needed by $concept")
          if (resource.isProgrammable && concept.placingConstraintsEquation(resource)) {
            logger.debug(s"${resource.name} could satisfy the needs for $concept")
            targets = targets + resource
          }
        }
      }
      if (targets.nonEmpty)
        concept.addProperty("targets", targets)
      else throw new NoTargetFoundException(concept) //If no target has been found for an concept, the policy can not be deployed
    }

    orderedGeneration(policy, topology)

    // Return the policy

    policy
  }


}

object Deploy {


  def apply(policy: Policy, topology: NetworkTopology, targets: Map[Concept, String]) = deploy(policy, topology, targets)
  def apply(policy: Policy, topology: NetworkTopology, heuristic: DeploymentRepartition) = {
    val tbegin = System.currentTimeMillis()
    val res = deploy(policy, topology, heuristic)
    println(s"\tTime Deploy: ${System.currentTimeMillis() - tbegin} ms")
    res
  }


  private def getNetworkFlows(policies:Set[Policy], ref:Policy) = {
    ref.flows -- policies.foldLeft(new Policy()){ (acc, e) => acc + e}.flows
  }


  /**
    * Manual deployment of a pre-deployed policy over a sensing infrastructure
    *
    * @param policy Pre-deployed policy
    * @param topology Network topology model
    * @param targets Manual association concept -> platform
    * @return A policy for each platform of the sensing infrastructure
    */
  def deploy (policy:Policy, topology: NetworkTopology, targets: Map[Concept, String]):Iterable[Policy] = {
    /**
      * Delete join points not involved in a network communication
      *
      * @param policy Policy
      * @return A policy without join points not involved in a network communication
      */
    def deleteNonRelevantJoinPoints(policy:Policy) = {

      val joinPoints = policy.ios.toList.collect{case x:JoinPoint[_] => x}.filterNot(_.hasProperty("network").isDefined)
      var sanitizedPolicy = policy
      joinPoints.foreach(c => sanitizedPolicy = sanitizedPolicy.delete(c))
      // Recopy properties
      policy.properties.foreach {p => sanitizedPolicy.addProperty(p.name, p.value)}
      sanitizedPolicy
    }

    // Associating a concept to a platform
    val projection = targets.map(t => (t._1, topology.resources.find(r => r.name equals t._2 ).get))
    val projectionGrouped = projection.toSeq.groupBy(_._2).map { e => e._1 -> e._2.map(_._1)}

    // Split the global policies into sub-policy and extend it with Join Points
    val tbegin0 = System.currentTimeMillis()
    val rawPolicies = projectionGrouped.map {e =>
      val extendedpolicy =  ExtendPolicy(policy.select(e._2.toSet, policy.name + "_" + e._1.name), onlyEmptyPorts = false)
      extendedpolicy.addProperty("board", e._1.name)
      extendedpolicy.addProperty("board_type", e._1.eType.toString)
      extendedpolicy.addProperty("generator", Features.codeGeneratorAssociation(e._1.language))
      extendedpolicy
    }
    println(s"\t\t\tSub-policies spliting: ${System.currentTimeMillis() - tbegin0} ms")

    // Compute which join points are network-related
    val networkFlows = getNetworkFlows(rawPolicies.toSet, policy)


    val tbegin = System.currentTimeMillis()
    // Associating same network id to linked join points
    for (l <- networkFlows.groupBy(_.source_output); src = l._1; dsts = l._2.map(_.destination_input)) {

      val uid = scala.util.Random.alphanumeric.take(5).mkString
      val optionJoinPointOutput = rawPolicies.find(aPolicy => aPolicy.concepts contains src.parent).get.nextElements(src.parent).collect {case (x:JoinPointOutput[_], _) => x}.find(_.fromConceptOutput == src)
      if (optionJoinPointOutput.isDefined) optionJoinPointOutput.get.addProperty("network",uid)
      for (dst <- dsts) {
        val optionJoinPointInput = rawPolicies.find(aPolicy => aPolicy.concepts contains dst.parent).get.previousElements(dst.parent).collect {case (x:JoinPointInput[_], _) => x}.find(_.toConceptInput == dst)
        if (optionJoinPointInput.isDefined) optionJoinPointInput.get.addProperty("network",uid)
      }
    }
    println(s"\t\t\tAssociating network id: ${System.currentTimeMillis() - tbegin} ms")
    // Delete non-relevant join points
    val readyToDeployedPolicies = rawPolicies.map { deleteNonRelevantJoinPoints }
    readyToDeployedPolicies.foreach(p => ToGraphviz.writeSource(p))

    readyToDeployedPolicies

  }

  /**
    * Deploy a policy over a sensing infrastructure according a heuristic
    *
    * @param policy Pre-deployed policy
    * @param topology Network topology model
    * @param heuristic Manual association concept -> platform
    * @return A policy for each platform of the sensing infrastructure
    */
  def deploy(policy: Policy, topology: NetworkTopology, heuristic: DeploymentRepartition):Iterable[Policy] = {
    val tbegin = System.currentTimeMillis()

    val targets = policy.concepts.par.map { c =>
      val place = heuristic.place(c, topology)
      c -> place.name
    }.toMap[Concept, String]
    println(s"\t\tTime for placing: ${System.currentTimeMillis() - tbegin} ms")
    deploy(policy, topology, targets.seq)
  }
}
