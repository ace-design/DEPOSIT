package fr.unice.modalis.cosmic.deposit.core


import com.typesafe.scalalogging.LazyLogging
import fr.unice.modalis.cosmic.deployment.generator.{CodeGenerator, ProcessingGenerator, PythonGenerator}
import fr.unice.modalis.cosmic.deposit.converter.{ToGraph, ToGraphviz}

import scala.collection.mutable.ArrayBuffer



/**
  * Policy definition
  * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
  *
  * @param operations Workflow element list
  * @param flows Link list
  */
case class Policy(var name:String, ios:Set[PolicyIO[_<:DataType]], operations:Set[Operation[_<:DataType,_<:DataType]], flows:Set[Flow[_<:DataType]]) extends Properties{


  // Constructors
  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("policy" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  // Sources and Collectors (lazy computation)
  lazy val sources = ios.filter(_.isInstanceOf[DataInput[_<:DataType]]).asInstanceOf[Set[DataInput[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[DataOutput[_<:DataType]]).asInstanceOf[Set[DataOutput[_<:DataType]]]
  lazy val sensors = ios.filter(_.isInstanceOf[Sensor[_<:DataType]]).asInstanceOf[Set[Sensor[_<:DataType]]]
  lazy val inputJoinPoints = ios.filter(_.isInstanceOf[JoinPointInput[_<:DataType]]).asInstanceOf[Set[JoinPointInput[_<:DataType]]]
  lazy val outputJoinPoints = ios.filter(_.isInstanceOf[JoinPointOutput[_<:DataType]]).asInstanceOf[Set[JoinPointOutput[_<:DataType]]]
  lazy val outputs = collectors ++ outputJoinPoints
  lazy val inputs = sources ++ inputJoinPoints
  lazy val concepts = ios ++ operations
  lazy val isExtendable = inputJoinPoints.nonEmpty || outputJoinPoints.nonEmpty

  /**
    * Find a concept by its id
    *
    * @param id Concept if
    * @return Result of id-lookup
    */
  def findConceptById(id:String) = concepts find {_.id equals id}

  /**
    * Find a sensor by its name
    *
    * @param name Sensor name
    * @return Result of name-lookup
    */
  def findSensorByName(name:String) = sensors find {_.name equals name}

  /**
    * Graph representation
    *
    * @return A Graph representation of this policy
    */
  def toGraph = ToGraph(this)

  /**
    * Add a concept in the policy
    *
    * @param concept Concept to add
    * @return A new policy with the concept added
    */
  def add(concept:Concept):Policy = {
    concept match {
      case n:PolicyIO[_] => addIO(n)
      case n:Operation[_, _] => addOperation(n)
      case _ => throw new IllegalArgumentException(concept + " is not handled by method add")
    }
  }

  /**
    * Add a flow in the policy
    *
    * @param flow Flow to add
    * @return A new policy with the flow added
    */
  def add(flow: Flow[_<:DataType]):Policy = {
    addFlow(flow)
  }

  /**
    * Delete a concept in the policy (remove also flows from and to this concepts)
    *
    * @param concept Concept to delete
    * @return A new policy with the concept and relatives flows deleted
    */
  def delete(concept:Concept):Policy = {
    concept match {
      case n:PolicyIO[_] => deleteIO(n)
      case n:Operation[_, _] => deleteActivity(n)
      case _ => throw new IllegalArgumentException(concept + " is not handled by method delete")
    }
  }

  /**
    * Delete a flow in the policy
    *
    * @param flow Flow to delete
    * @return A new policy with the flow deleted
    */
  def delete(flow: Flow[_<:DataType]):Policy = {
    deleteFlow(flow)
  }

  /**
    * Add an I/O to the policy
    *
    * @param io I/O to add
    * @return A new policy with the I/O added
    */
  def addIO(io:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios + io, operations, flows)
  }

  /**
    * Add an operation in the current workflow
    *
    * @param operation Operation to add
    * @return A new policy with the operation added
    */
  def addOperation[T<:DataType, O<:DataType](operation:Operation[T,O]):Policy  = {
    operation match {
      case Process(wf, _, _) => new Policy(name, ios, operations + operation, flows ++ autoConnectProcess(operation.asInstanceOf[Process[_<:DataType, _<:DataType]]))
      case _ => new Policy(name, ios, operations + operation, flows)
    }
  }

  /**
    * Add a flow in the current policy
    *
    * @param flow Flow
    * @return A new policy with the flow added
    */
  def addFlow(flow:Flow[_<:DataType]):Policy  = {
    new Policy(name, ios, operations, flows + flow)
  }

  /**
    * Delete an operation in the current workflow (Delete also all flows referring this activity)
    *
    * @param operation Operation to delete
    * @return A policy without this operation and flows referring this operation
    */
  def deleteActivity(operation:Operation[_<:DataType,_<:DataType]):Policy = {
    new Policy(name, ios, operations - operation, flows.filterNot(p => (p.destination == operation) || (p.source == operation)))
  }

  /**
    * Delete an IO in the current policy (/!\ Delete also all flows referring this IO)
    *
    * @param io I/O to delete
    * @return A policy without this IO and flows referring this IO
    */
  def deleteIO(io:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios - io, operations, flows.filterNot(p => (p.destination == io) || (p.source == io)))
  }

  /**
    * Delete a flow in the current policy
    *
    * @param flow Flow to delete
    * @return A new policy with the flow removed
    */
  def deleteFlow(flow:Flow[_<:DataType]):Policy  = {
    new Policy(name, ios, operations, flows - flow)
  }


  /**
    * Compute the sub-policy between a root and a leaf
    *
    * @param root Root concept
    * @param leaf Leaf concept
    * @return A sub-policy corresponding to the extraction of concepts and flows between the root and the leaf or empty policy if no path has been found
    */
  def subPolicy(root:Concept, leaf:Concept):Policy = {
    val theGraph = toGraph

    val nRoot = theGraph get root
    val nLeaf = theGraph get leaf

    val path = nRoot pathTo nLeaf

    if (path.isDefined){
      val nflows = path.get.edges.toList.flatMap{e => getFlowsBetween(e.from, e.to)}
      val nconcepts = nflows.flatMap{e => List(e.source, e.destination)}
      Policy(s"sub_${this.name}", nconcepts.collect{case x:DataIO[_] => x}.toSet, nconcepts.collect{case x:Operation[_,_] => x}.toSet, nflows.toSet)
    } else {
      new Policy()
    }
  }

  /**
    * Compute the sub-policy between a root and all collectors
    *
    * @param root Root concept
    * @return A sub-policy corresponding to the extraction of concepts and flows between the root and collectors or empty policy if no path has been found
    */
  def subPolicy(root:Concept):Policy = {
    this.collectors.map{c => subPolicy(root, c)}.reduceLeft(_ + _)//.foldLeft(new Policy())( _ + _)
  }

  /**
    * Find the flows having the specified source and destination
    *
    * @param source Source concept
    * @param destination Destination concept
    * @return A set of flows having the specified source and destination
    */
  def getFlowsBetween(source:Concept, destination:Concept) = flows.filter {f => (f.source equals source) && (f.destination equals destination)}

  /**
    * Find the next policy concepts
    *
    * @param concept Current policy concept
    * @return A set of (Concept,Flow) referring to the next concepts and flows
    */
  def nextElements(concept:Concept):Set[(Concept, Flow[_<:DataType])] = {
    var res = Set[(Concept, Flow[_<:DataType])]()
    for (l <- flows) {
      if (l.source == concept)
        res = res ++ flows.filter(l => l.source == concept).foldLeft(Set.empty[(Concept, Flow[_<:DataType])]){ (acc, e) => acc.+((e.destination, l))}
    }
    res
  }

  /**
    * Find the previous policy concepts
    *
    * @param concept Current policy concept
    * @return A set of (Concept,Flow) referring to the previous concepts and flows
    */
  def previousElements(concept:Concept):Set[(Concept, Flow[_<:DataType])] = {
    var res = Set[(Concept, Flow[_<:DataType])]()
    for (l <- flows) {
      if (l.destination == concept)
        res = res ++ flows.filter(l => l.destination == concept).foldLeft(Set.empty[(Concept, Flow[_<:DataType])]){ (acc, e) => acc.+((e.source, l))}
    }
    res
  }


  /**
    * Generate flows required to connect a process with current known sensors in a policy
    *
    * @param process Process operation
    * @tparam I Input Data type
    * @tparam O Output Data type
    * @return A set of flows needed to connect the process with the current known sensors
    */
  private def autoConnectProcess[I<:DataType, O<:DataType](process:Process[I,O]) = {
    var flows = new ArrayBuffer[Flow[_<:DataType]]()
    process.inputsNames.foreach(i => {
      val possibleSensor = ios.filter(p => p.isInstanceOf[Sensor[I]]).find(s => s.asInstanceOf[Sensor[I]].url == i)
      possibleSensor match {
        case Some(n) => flows += new Flow(n.asInstanceOf[Sensor[I]].output, process.getInput(i))
        case None => /* Nop */

      }
    }
    )
    flows.toSet
  }


  /**
    * Return the set of sensors needed for a concept
    *
    * @param concept Concept
    * @return A sensor set
    */
  def sensorsInvolved(concept: Concept):Set[Sensor[_<:DataType]] = {
    var visited = List[Concept]()
    def inner(c:Concept):List[Sensor[_<:DataType]] = {
      c match {
        case n:Sensor[_] => List(n)
        case n:Concept => flowsTo(n).map(_.source).foldLeft(List[Sensor[_<:DataType]]()){ (acc, c) => if (!visited.contains(c)) {visited = c :: visited; inner(c) ::: acc} else acc}

      }
    }
    inner(concept).toSet
  }
  /**
    * Select operator
    *
    * @param n New policy's name
    * @param concepts Set of concepts
    * @return A new policy containing only selected concepts
    */
  def select(concepts:Set[Concept], n:String = "select_") = {
    val flows = this.flows.filter(f => (concepts contains f.source) && (concepts contains f.destination))
    Policy(n, concepts collect {case x:PolicyIO[_] => x}, concepts collect {case x:Operation[_,_] => x}, flows)
  }

  /**
    * Data types involved in a policy
    *
    * @return A set of data types involved
    */
  def getInvolvedDataTypes[T<:DataType] = {
    ios.map {_.dataType} ++ operations.map {_.iType} ++ operations.map {_.oType}
  }

  /**
    * Input Ports that are not connected by a flow
    *
    * @return A list of non-connected flows
    */
  def getNonConnectedInputPorts = {
    // Compute list of input ports
    val ports = concepts.foldLeft(List[Port[_]]()) { (acc, e) => e match {
      case x:DataOutput[_] => x.input :: acc
      case x:JoinPointOutput[_] => x.input :: acc
      case x:Operation[_, _] => x.inputs.toList ::: acc
      case _ => acc
    }}.toSet

    val portsInUse = flows.foldLeft(List[Port[_]]()) { (acc, e) => e.destination_input :: acc}.toSet

    ports -- portsInUse

  }

  /**
    * Input Ports that are not connected by a flow
    *
    * @return A list of non-connected flows
    */
  def getNonConnectedOutputPorts = {
    // Compute list of input ports
    val ports = concepts.foldLeft(List[Port[_]]()) { (acc, e) => e match {
      case x:DataInput[_] => x.output :: acc
      case x:JoinPointInput[_] => x.output :: acc
      case x:Operation[_, _] => x.outputs.toList ::: acc
      case _ => acc
    }}.toSet

    val portsInUse = flows.foldLeft(List[Port[_]]()) { (acc, e) => e.source_output :: acc}.toSet

    ports -- portsInUse

  }

  /**
    * Duplicate a policy
    *
    * @return A duplicated policy
    */
  def duplicate = {
    // Convert flows between operations
    val corr = (this.flows.map { l => l.source } ++ this.flows.map{ l => l.destination}).map{ c => (c, c.duplicate)}.toMap

    // Build new flows
    val _l1 = this.flows.filter(l => l.source.isInstanceOf[Operation[_,_]] && l.destination.isInstanceOf[Operation[_,_]]).map{ l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Flow(l._1.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(l._3), l._2.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(l._4)))
    val _l2 = this.flows.filter(l => l.source.isInstanceOf[DataInput[_]] && l.destination.isInstanceOf[Operation[_,_]]).map{ l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Flow(l._1.asInstanceOf[DataInput[_<:DataType]].output, l._2.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(l._4)))
    val _l3 = this.flows.filter(l => l.source.isInstanceOf[Operation[_,_]] && l.destination.isInstanceOf[DataOutput[_]]).map{ l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Flow(l._1.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(l._3), l._2.asInstanceOf[DataOutput[_<:DataType]].input))
    val _l4 = this.flows.filter(l => l.source.isInstanceOf[DataInput[_]] && l.destination.isInstanceOf[DataOutput[_]]).map{ l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Flow(l._1.asInstanceOf[DataInput[_<:DataType]].output, l._2.asInstanceOf[DataOutput[_<:DataType]].input))
    val linksDuplicated = _l1 ++ _l2 ++ _l3 ++ _l4
    val iosDuplicated = linksDuplicated.filter(_.source.isInstanceOf[DataIO[_]]).map {_.source} ++ linksDuplicated.filter(_.destination.isInstanceOf[DataIO[_]]).map {_.destination}
    val operationsDuplicated = linksDuplicated.filter(_.source.isInstanceOf[Operation[_,_]]).map {_.source} ++ linksDuplicated.filter(_.destination.isInstanceOf[Operation[_,_]]).map {_.destination}

    new Policy(name,iosDuplicated.asInstanceOf[Set[PolicyIO[_<:DataType]]], operationsDuplicated.asInstanceOf[Set[Operation[_<:DataType, _<:DataType]]], linksDuplicated.asInstanceOf[Set[Flow[_<:DataType]]])

  }

  /**
    * Compute flows going to a concept
    *
    * @param concept Concept
    * @return List of in-going flows
    */
  def flowsTo(concept:Concept) = flows.filter(l => l.destination == concept)

  /**
    * Compute flows going from a concept
    *
    * @param concept Concept
    * @return List of out-going flows
    */
  def flowsFrom(concept:Concept) = flows.filter(l => l.source == concept)

  /**
    * Compose the current policy with an other one
    *
    * @param other Other policy
    * @return A new policy resulted from the composition of the current one and the one specified as a parameter
    */
  def +(other:Policy):Policy = Policy.compose(this, other)

  override def toString = "Policy[name=" + name + ";ios={" + ios + "};operations={" + operations + "};flows={" + flows + "}]"


  def exportToWiring():Unit = ProcessingGenerator(this, toFile = true)
  def exportToPython():Unit = PythonGenerator(this, toFile = true)

  /**
    * Export the policy to graphviz
    */
  def exportToGraphviz():Unit = ToGraphviz.writeSource(this)

  def target():String = {
    if (hasProperty("board").isDefined) {
      readProperty("board").get.asInstanceOf[String]
    } else throw new Exception(s"No target has been found for $name (perhaps the policy has not been predeployed")
  }

  def generate():Unit = {
    if (hasProperty("generator").isDefined) {
      val generator = readProperty("generator").get.asInstanceOf[CodeGenerator]
      generator.apply(this, toFile = true)
    } else throw new Exception(s"No code generator has been found for $name (perhaps the policy has not been predeployed)")
  }
}

object Policy extends LazyLogging{



  case class NonValidPolicyException(policy:Policy, reason:String) extends Exception(policy.name + s" is not valid because $reason")

  /**
    * Check if the policy satisfies the validity properties. Returns nothing if the policy is valid but throws NonValidPolicyException if the policy is not valid
    *
    * @param policy Policy
    */
  def checkValidity(policy: Policy):Unit = {
    // A policy should have sensors and collectors
    if (policy.sensors.isEmpty) throw NonValidPolicyException(policy, "has no sensors")
    if (policy.collectors.isEmpty) throw NonValidPolicyException(policy, "has no collectors")

    // A policy should not have non-connected input ports
    if (policy.getNonConnectedInputPorts.nonEmpty) throw NonValidPolicyException(policy, "has empty input ports")

    // An operation input is connected by only one flow
    if (!policy.operations.forall(operation => operation.inputs.forall(input => policy.flows.count(_.destination_input equals input) == 1))) throw NonValidPolicyException(policy, "has an input port has more than one incoming data flow")

    // A policy sink is a data output
    if (policy.operations.exists(o => !policy.flows.exists(p => p.source == o))) throw NonValidPolicyException(policy, "has a sink operation")

    // A policy is acyclic
    if (policy.toGraph.isCyclic) throw NonValidPolicyException(policy, "has a cycle")

    // A policy has no loops
    if (policy.flows.exists {f => f.source == f.destination}) throw NonValidPolicyException(policy, "has a loop on a concept")

    // Runtime invariant
    if (policy.flows.exists {f => !policy.concepts.contains(f.source) || !policy.concepts.contains(f.destination)}) throw NonValidPolicyException(policy, "has an inconsistency between concepts and flows")
  }

  def mergeActivities(p:Policy) = {

    def similarities(list:List[Operation[_<:DataType,_<:DataType]]):Map[Operation[_<:DataType,_<:DataType], List[Operation[_<:DataType,_<:DataType]]] = {
      list match {
        case a :: tail => Map(a -> tail.filter(_ ~= a)) ++ similarities(tail)
        case a :: Nil => Map(a -> List())
        case Nil => Map()
      }
    }

    val candidates = similarities(p.operations.toList).filter(_._2.nonEmpty)

    val res = candidates.map { c =>
      val inFlows = p.flowsTo(c._1).map{f => (f.source_output.name, f.destination_input.name)}
      c._1 -> c._2.filter(f => p.flowsTo(f).map{f => (f.source_output.name, f.destination_input.name)} == inFlows)
    }

    val newFlows = res.flatMap { c =>
      c._2.flatMap{f => p.flowsFrom(f).map{e => new Flow(c._1.getOutput(e.source_output.name), e.destination_input)}}
    }

    var policy = p
    res.foreach(e => e._2.foreach(f => policy = policy.delete(f)))

    newFlows.foreach(f => policy = policy.addFlow(f))

    policy
  }

  /**
    * Compose two policies together
    *
    * @param p1 Data collection policy
    * @param p2 Data collection policy
    * @return A composed data collection policy
    */
  def compose(p1:Policy, p2:Policy) = {

    def composeName(p1:Policy, p2:Policy):String = {
      (p1.name, p2.name) match {
        case ("", a) if a.nonEmpty => a
        case (a, "") if a.nonEmpty => a
        case (a, b) if a.nonEmpty && b.nonEmpty => s"comp_${System.currentTimeMillis}"
        case ("", "") => "policy" + scala.util.Random.alphanumeric.take(5).mkString
      }
    }



    def sensorFusion(p1:Policy, p2:Policy):Policy = {
      logger.info(s"Prepare composition of ${p1.name} with ${p2.name}")
      var compose = Policy(composeName(p1, p2), p1.ios ++ p2.ios, p1.operations ++ p2.operations, p1.flows ++ p2.flows)

      logger.debug(s"Compose ios: ${compose.ios}")

      val similarities = p2.sensors.map { s2 => (s2, p1.sensors.find(s1 => (s1 ~= s2) && (s1 != s2))) }

      logger.info(s"Similarities found: $similarities")

      val flows = for (s <- similarities.filter(_._2.isDefined)) yield (s._1, p1.flowsFrom(s._2.get))
      val flowsToAdd = flows.flatMap{couple => couple._2.map{f => new Flow(couple._1.output, f.destination_input)}}

      logger.info(s"Flows to add: $flowsToAdd")

      similarities.filter(_._2.isDefined).foreach(e => {
        logger.debug(s"Deleting ${e._2.get}")
        compose = compose.delete(e._2.get)
      })

      flowsToAdd.foreach(f => {
        logger.debug(s"Adding $f")
        compose = compose.addFlow(f)
      })

      println(s"Result: ${compose.sensors}")
      compose
    }
    /*def sensorFusion(p1:Policy, p2:Policy):Policy = {
      // Find similar sensors in p2
      logger.debug("Sensors in p1:" + p1.sensors)
      logger.debug("Sensors in p2:" + p2.sensors)

      // If there are sensors in p1 and p2
      if (p2.sensors.nonEmpty && p1.sensors.nonEmpty) {
        // Return similar sensors contained in p1 and p2
        val similar = p2.sensors.map {s => (s, p1.sensors.find(_ ~= s))}
        logger.debug(s"Simular sensors: $similar")

        if (similar.forall(_._2.isEmpty)){
          // If there is no similar sensors, return a trivial composition result
          new Policy(composeName(p1, p2), p1.ios ++ p2.ios, p1.operations ++ p2.operations, p1.flows ++ p2.flows)
        }
        else
        {
          val newFlows = for (couple <- similar) yield {
            val sensorinP2 = couple._1
            val targetinP2 = p2.flows.find(_.source == sensorinP2).get
            if (couple._2.isDefined){
              val sensorinP1 = couple._2.get
              Some(new Flow(sensorinP1.output, targetinP2.destination_input))
            } else None
          }

        logger.debug("New flows resulting from sensor fusion: " + newFlows.flatten)

          // Return a policy where sensors has been fused
          // Delete similar sensors in p2
          var p2withoutSimilarities = p2
          similar.map{_._1}.foreach(s => p2withoutSimilarities = p2withoutSimilarities.delete(s))
          Policy(composeName(p1, p2),
                p1.ios ++ p2withoutSimilarities.ios,
                p1.operations ++ p2withoutSimilarities.operations,
                p1.flows ++ p2withoutSimilarities.flows ++ newFlows.flatten)
        }
      }
        // If there are no sensors in p1 and p2, abort the sensor fusion
      else new Policy(composeName(p1, p2), p1.ios ++ p2.ios, p1.operations ++ p2.operations, p1.flows ++ p2.flows)
    }*/


    logger.debug(s"Prepare to compose ${p1.name}  ({${p1.sensors.map(_.name)}) with ${p2.name} ({${p2.sensors.map(_.name)})")
    //mergeActivities(sensorFusion(p1, p2))
    sensorFusion(p1,p2)

  }
}