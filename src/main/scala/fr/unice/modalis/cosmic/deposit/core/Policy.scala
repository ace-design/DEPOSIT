package fr.unice.modalis.cosmic.deposit.core


import fr.unice.modalis.cosmic.deployment.generator.{ProcessingGenerator, PythonGenerator}
import fr.unice.modalis.cosmic.deposit.converter.{ToGraph, ToGraphviz}

import scala.collection.mutable.ArrayBuffer


/**
 * Policy definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param operations Workflow element list
 * @param flows Link list
 */
case class Policy(var name:String, ios:Set[PolicyIO[_<:DataType]], operations:Set[Operation[_<:DataType,_<:DataType]], flows:Set[Flow[_<:DataType]]) extends Properties{


  // Constructors
  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

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

  def findConceptById(id:String) = concepts find {_.id equals id}

  def hasPeriodicSensors = sources.collect{case x:PeriodicSensor[_] => x}.nonEmpty

  /**
   * Graph representation
   * @return A Graph representation of this workflow
   */
  def graph = ToGraph(this)

  def add(c:Concept):Policy = {
    c match {
      case n:PolicyIO[_] => addIO(n)
      case n:Operation[_, _] => addActivity(n)
      case _ => throw new IllegalArgumentException(c + " is not handled by method add")
    }
  }

  def delete(c:Concept):Policy = {
    c match {
      case n:PolicyIO[_] => deleteIO(n)
      case n:Operation[_, _] => deleteActivity(n)
      case _ => throw new IllegalArgumentException(c + " is not handled by method delete")
    }
  }

  def addIO(o:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios + o, operations, flows)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity[T<:DataType, O<:DataType](c:Operation[T,O]):Policy  = {
    c match {
      case Process(wf, _, _) => new Policy(name, ios, operations + c, flows ++ autoConnectProcess(c.asInstanceOf[Process[_<:DataType, _<:DataType]]))
      case _ => new Policy(name, ios, operations + c, flows)
    }
  }

  /**
   * Add a flow in the current workflow
   * @param l Flow
   * @return A new workflow with the flow added
   */
  def addFlow(l:Flow[_<:DataType]):Policy  = {
   new Policy(name, ios, operations, flows + l)
  }

  /**
   * Delete an activity in the current workflow (/!\ Delete also all flows referring this activity)
   * @param c Workflow activity
   * @return A workflow without this activity and flows referring this activity
   */
  def deleteActivity(c:Operation[_<:DataType,_<:DataType]):Policy = {
    new Policy(name, ios, operations - c, flows.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Delete an IO in the current workflow (/!\ Delete also all flows referring this IO)
   * @param c Workflow IO
   * @return A workflow without this IO and flows referring this IO
   */
  def deleteIO(c:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios - c, operations, flows.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Return sub Workflow
   * @param root Root element
   */
  def subWorkflow(root:Concept, last:Option[Concept] = None):Policy = {
    val ios = new ArrayBuffer[PolicyIO[_<:DataType]]()
    val activities = new ArrayBuffer[Operation[_<:DataType, _<:DataType]]()
    val flows = new ArrayBuffer[Flow[_<:DataType]]()
    // Add root into the activities/ios

    root match {
      case elem:PolicyIO[_] => ios += elem
      case elem:Operation[_, _] => activities += elem
    }

    def internal(e:Concept):Unit = {
      val next = nextElements(e)
      next.foreach(e => e._1 match {
        case elem:Collector[_]  => ios += elem; flows += e._2
        case elem:Sensor[_] =>  ios += elem; flows += e._2; if (e != Set.empty && last.isEmpty || last.get != e._1) internal(e._1)
        case elem:Operation[_, _] => activities += elem; flows += e._2; if (e != Set.empty && last.isEmpty || last.get != e._1) internal(e._1)
      }
      )
    }
    internal(root)
    new Policy("sub" + name, ios.toSet, activities.toSet, flows.toSet)
  }

  /**
   * Find the next workflow elements
   * @param e Current workflow element
   * @return Immediate next elements
   */
  def nextElements(e:Concept):Set[(Concept, Flow[_<:DataType])] = {
    var res = Set[(Concept, Flow[_<:DataType])]()
    for (l <- flows) {
      if (l.source == e)
        res = res ++ flows.filter(l => l.source == e).foldLeft(Set.empty[(Concept, Flow[_<:DataType])]){ (acc, e) => acc.+((e.destination, l))}
    }
    res
  }

  def previousElements(e:Concept):Set[(Concept, Flow[_<:DataType])] = {
    var res = Set[(Concept, Flow[_<:DataType])]()
    for (l <- flows) {
      if (l.destination == e)
        res = res ++ flows.filter(l => l.destination == e).foldLeft(Set.empty[(Concept, Flow[_<:DataType])]){ (acc, e) => acc.+((e.source, l))}
    }
    res
  }

  /**
   * Delete a flow in the current workflow
   * @param l Link
   * @return A workflow with the flow removed
   */
  def deleteFlow(l:Flow[_<:DataType]):Policy  = {
    new Policy(name, ios, operations, flows - l)
  }

  /**
   * Generate WFLinks to connect a process with current known sensors in a workflow
   * @param p Process
   * @tparam I Input Data type
   * @tparam O Output Data type
   * @return A set ok flows needed to connect the process with the current known sensors
   */
  private def autoConnectProcess[I<:DataType, O<:DataType](p:Process[I,O]) = {
    var flows = new ArrayBuffer[Flow[_<:DataType]]()
    p.inputsNames.foreach(i => {
      val possibleSensor = ios.filter(p => p.isInstanceOf[Sensor[I]]).find(s => s.asInstanceOf[Sensor[I]].url == i)
      possibleSensor match {
        case Some(n) => flows += new Flow(n.asInstanceOf[Sensor[I]].output, p.getInput(i))
        case None => /* Nop */

      }
    }
    )
    flows.toSet
  }


  /**
   * Return the set of sensors needed for a concept
   * @param c Concept
   * @return A sensor set
   */
  def sensorsInvolved(c: Concept):Set[Sensor[_<:DataType]] = {
    var visited = List[Concept]()
    def inner(c:Concept):List[Sensor[_<:DataType]] = {
      c match {
        case n:Sensor[_] => List(n)
        case n:Concept => flowsTo(n).map(_.source).foldLeft(List[Sensor[_<:DataType]]()){ (acc, c) => if (!visited.contains(c)) {visited = c :: visited; inner(c) ::: acc} else acc}

      }
    }
    inner(c).toSet
  }
  /**
   * Select operator
   * @param n New policy's name
   * @param e Set of concepts
   * @return A new policy containing only selected concepts
   */
  def select(e:Set[Concept], n:String = "select_") = {
    var result = this
    val notSelected = result.concepts -- e
    notSelected.foreach(c => result = result.delete(c))
    result.name = n
    result
  }

  /**
   * Return data types involved in a policy
   * @return A set of data types involved
   */
  def dataTypesInvolved[T<:DataType] = {
    ios.map {_.dataType} ++ operations.map {_.iType} ++ operations.map {_.oType}
  }

  def nonConnectedPorts = {
    // Compute list of ports
    val ports = concepts.foldLeft(List[Port[_]]()) { (acc, e) => e match {
      case x:DataInput[_] => x.output :: acc
      case x:DataOutput[_] => x.input :: acc
      case x:JoinPointInput[_] => x.output :: acc
      case x:JoinPointOutput[_] => x.input :: acc
      case x:Operation[_, _] => x.inputs.toList ::: x.outputs.toList ::: acc
    }}.toSet

    val portsInUse = flows.foldLeft(List[Port[_]]()) { (acc, e) => e.destination_input :: e.source_output :: acc}.toSet

    ports -- portsInUse

  }
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
  def flowsTo(a:Concept) = flows.filter(l => l.destination == a)

  def flowsFrom(a:Concept) = flows.filter(l => l.source == a)

  def ++(w:Policy):Policy = Policy.fuse(this, w)//new Policy(this.name + "_" + w.name, this.ios ++ w.ios, this.operations ++ w.operations, this.flows ++ w.flows)

  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + operations + "};links={" + flows + "}]"

  def exportToWiring = ProcessingGenerator(this, toFile = true)
  def exportToPython = PythonGenerator(this, toFile = true)
  def exportToGraphviz = ToGraphviz.writeSource(this)
}

object Policy {
  def fuse(p1:Policy, p2:Policy) = {
    println("Prepare to compose " + p1.name + " with " + p2.name)
    // Find similar sensors in p2
    println("Sensors in p1:" + p1.sensors)
    println("Sensors in p2:" + p2.sensors)
    if (p2.sensors.nonEmpty && p1.sensors.nonEmpty) {

      val similar = p2.sensors.map {s => (s, p1.sensors.find(_ ~= s))}
      if (similar.forall(_._2.isEmpty))
        new Policy(p1.name + "_" + p2.name, p1.ios ++ p2.ios, p1.operations ++ p2.operations, p1.flows ++ p2.flows)
      else {
        val newFlows = for (f <- p2.flows) yield {
          if (similar.map {
            _._1.asInstanceOf[Concept]
          }.contains(f.source)) {
            if (similar.exists(_._1 equals f.source.asInstanceOf[Sensor[_ <: DataType]])) {
              val res = similar.find(_._1 equals f.source.asInstanceOf[Sensor[_ <: DataType]]).get._2
              if (res.isDefined) Some(Flow(res.get.output, f.destination_input)) else None
            }
            else None
          } else None
        }
        println(newFlows.flatten)

        // Delete similar sensors in p2
        var p2withoutSimiarities = p2
        similar.map{_._1}.foreach(s => p2withoutSimiarities = p2.delete(s))
        Policy(p1.name + "_" + p2withoutSimiarities.name, p1.ios ++ p2withoutSimiarities.ios, p1.operations ++ p2withoutSimiarities.operations, p1.flows ++ p2withoutSimiarities.flows ++ newFlows.flatten)
      }


    }
    else new Policy(p1.name + "_" + p2.name, p1.ios ++ p2.ios, p1.operations ++ p2.operations, p1.flows ++ p2.flows)
  }
}