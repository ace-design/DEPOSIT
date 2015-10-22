package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deposit.converter.ToGraph

import scala.collection.mutable.ArrayBuffer


/**
 * Policy definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param operations Workflow element list
 * @param links Link list
 */
case class Policy(var name:String, ios:Set[PolicyIO[_<:DataType]], operations:Set[Operation[_<:DataType,_<:DataType]], links:Set[Link[_<:DataType]]) extends Properties{


  // Constructors
  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  // Sources and Collectors (lazy computation)
  lazy val sources = ios.filter(_.isInstanceOf[DataInput[_<:DataType]]).asInstanceOf[Set[DataInput[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[DataOutput[_<:DataType]]).asInstanceOf[Set[DataOutput[_<:DataType]]]
  lazy val inputJoinPoints = ios.filter(_.isInstanceOf[JoinPointInput[_<:DataType]]).asInstanceOf[Set[JoinPointInput[_<:DataType]]]
  lazy val outputJoinPoints = ios.filter(_.isInstanceOf[JoinPointOutput[_<:DataType]]).asInstanceOf[Set[JoinPointOutput[_<:DataType]]]

  lazy val isExtendable = inputJoinPoints.nonEmpty || outputJoinPoints.nonEmpty

  addProperty("name", name) //Add name as a property


  def findConceptById(id:String) = ios ++ operations find {_.id equals id}

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
      case _ => throw new Exception(c + " is not handled by method add")
    }
  }

  def delete(c:Concept):Policy = {
    c match {
      case n:PolicyIO[_] => deleteIO(n)
      case n:Operation[_, _] => deleteActivity(n)
      case _ => throw new Exception(c + " is not handled by method delete")
    }
  }

  def addIO(o:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios + o, operations, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity[T<:DataType, O<:DataType](c:Operation[T,O]):Policy  = {
    c match {
      case Process(wf, _, _) => new Policy(name, ios, operations + c, links ++ autoConnectProcess(c.asInstanceOf[Process[_<:DataType, _<:DataType]]))
      case _ => new Policy(name, ios, operations + c, links)
    }
  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:Link[_<:DataType]):Policy  = {
   new Policy(name, ios, operations, links + l)
  }

  /**
   * Delete an activity in the current workflow (/!\ Delete also all links referring this activity)
   * @param c Workflow activity
   * @return A workflow without this activity and links referring this activity
   */
  def deleteActivity(c:Operation[_<:DataType,_<:DataType]):Policy = {
    new Policy(name, ios, operations - c, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Delete an IO in the current workflow (/!\ Delete also all links referring this IO)
   * @param c Workflow IO
   * @return A workflow without this IO and links referring this IO
   */
  def deleteIO(c:PolicyIO[_<:DataType]):Policy = {
    new Policy(name, ios - c, operations, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Return sub Workflow
   * @param root Root element
   */
  def subWorkflow(root:Concept, last:Option[Concept] = None):Policy = {
    val ios = new ArrayBuffer[PolicyIO[_<:DataType]]()
    val activities = new ArrayBuffer[Operation[_<:DataType, _<:DataType]]()
    val links = new ArrayBuffer[Link[_<:DataType]]()
    // Add root into the activities/ios

    root match {
      case elem:PolicyIO[_] => ios += elem
      case elem:Operation[_, _] => activities += elem
    }

    def internal(e:Concept):Unit = {
      val next = nextElements(e)
      next.foreach(e => e._1 match {
        case elem:Collector[_]  => ios += elem; links += e._2
        case elem:Sensor[_] =>  ios += elem; links += e._2; if (e != Set.empty && last.isEmpty || last.get != e._1) internal(e._1)
        case elem:Operation[_, _] => activities += elem; links += e._2; if (e != Set.empty && last.isEmpty || last.get != e._1) internal(e._1)
      }
      )
    }
    internal(root)
    new Policy("sub" + name, ios.toSet, activities.toSet, links.toSet)
  }

  /**
   * Find the next workflow elements
   * @param e Current workflow element
   * @return Immediate next elements
   */
  def nextElements(e:Concept):Set[(Concept, Link[_<:DataType])] = {
    var res = Set[(Concept, Link[_<:DataType])]()
    for (l <- links) {
      if (l.source == e)
        res = res ++ links.filter(l => l.source == e).foldLeft(Set.empty[(Concept, Link[_<:DataType])]){(acc, e) => acc.+((e.destination, l))}
    }
    res
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:Link[_<:DataType]):Policy  = {
    new Policy(name, ios, operations, links - l)
  }

  /**
   * Generate WFLinks to connect a process with current known sensors in a workflow
   * @param p Process
   * @tparam I Input Data type
   * @tparam O Output Data type
   * @return A set ok links needed to connect the process with the current known sensors
   */
  private def autoConnectProcess[I<:DataType, O<:DataType](p:Process[I,O]) = {
    var links = new ArrayBuffer[Link[_<:DataType]]()
    p.inputsNames.foreach(i => {
      val possibleSensor = ios.filter(p => p.isInstanceOf[Sensor[I]]).find(s => s.asInstanceOf[Sensor[I]].url == i)
      possibleSensor match {
        case Some(n) => links += new Link(n.asInstanceOf[Sensor[I]].output, p.getInput(i))
        case None => /* Nop */

      }
    }
    )
    links.toSet
  }

  def allInputs[T<:DataType] = {
    val array = ArrayBuffer[Input[_<:DataType]]()
    ios.collect{case x:Collector[T] => x}.foreach(c => array += c.input)
    operations.foreach(a => array ++= a.inputs)
    array.toSet
  }

  def allOutputs[T<:DataType] = {
    val array = ArrayBuffer[Output[_<:DataType]]()
    ios.collect{case x:Sensor[T] => x}.foreach(c => array += c.output)
    operations.foreach(a => array ++= a.outputs)
    array.toSet
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
        case n:Concept => linksTo(n).map(_.source).foldLeft(List[Sensor[_<:DataType]]()){(acc, c) => if (!visited.contains(c)) {visited = c :: visited; inner(c) ::: acc} else acc}

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
    val notSelected = (result.ios ++ result.operations) -- e
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

  def duplicate = {
    // Convert links between operations
    val corr = (this.links.map {l => l.source } ++ this.links.map{l => l.destination}).map{c => (c, c.duplicate)} toMap

    // Build new links /!\ This code will be refactored (dont care about the instance of)
    val _l1 = this.links.filter(l => l.source.isInstanceOf[Operation[_,_]] && l.destination.isInstanceOf[Operation[_,_]]).map{l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Link(l._1.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(l._3), l._2.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(l._4)))
    val _l2 = this.links.filter(l => l.source.isInstanceOf[DataInput[_]] && l.destination.isInstanceOf[Operation[_,_]]).map{l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Link(l._1.asInstanceOf[DataInput[_<:DataType]].output, l._2.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(l._4)))
    val _l3 = this.links.filter(l => l.source.isInstanceOf[Operation[_,_]] && l.destination.isInstanceOf[DataOutput[_]]).map{l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Link(l._1.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(l._3), l._2.asInstanceOf[DataOutput[_<:DataType]].input))
    val _l4 = this.links.filter(l => l.source.isInstanceOf[DataInput[_]] && l.destination.isInstanceOf[DataOutput[_]]).map{l => (corr(l.source), corr(l.destination), l.source_output.name, l.destination_input.name)}.map(l => Link(l._1.asInstanceOf[DataInput[_<:DataType]].output, l._2.asInstanceOf[DataOutput[_<:DataType]].input))
    val linksDuplicated = _l1 ++ _l2 ++ _l3 ++ _l4
    val iosDuplicated = linksDuplicated.filter(_.source.isInstanceOf[DataIO[_]]).map {_.source} ++ linksDuplicated.filter(_.destination.isInstanceOf[DataIO[_]]).map {_.destination}
    val operationsDuplicated = linksDuplicated.filter(_.source.isInstanceOf[Operation[_,_]]).map {_.source} ++ linksDuplicated.filter(_.destination.isInstanceOf[Operation[_,_]]).map {_.destination}

    new Policy(name,iosDuplicated.asInstanceOf[Set[PolicyIO[_<:DataType]]], operationsDuplicated.asInstanceOf[Set[Operation[_<:DataType, _<:DataType]]], linksDuplicated.asInstanceOf[Set[Link[_<:DataType]]])

  }
  def linksTo(a:Concept) = links.filter(l => l.destination == a)

  def linksFrom(a:Concept) = links.filter(l => l.source == a)

  def ++(w:Policy):Policy = new Policy(this.name + "_" + w.name, this.ios ++ w.ios, this.operations ++ w.operations, this.links ++ w.links)

  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + operations + "};links={" + links + "}]"

}

