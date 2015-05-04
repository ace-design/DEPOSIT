package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.converter.ToGraph

import scala.collection.mutable.ArrayBuffer


/**
 * Policy definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param operations Workflow element list
 * @param links Link list
 */
case class Policy(name:String, ios:Set[DataIO[_<:DataType]], operations:Set[Operation[_<:DataType,_<:DataType]], links:Set[Link[_<:DataType]]) {


  // Constructors
  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  // Sources and Collectors (lazy computation)
  lazy val sources = ios.filter(_.isInstanceOf[Sensor[_<:DataType]]).asInstanceOf[Set[Sensor[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[Collector[_<:DataType]]).asInstanceOf[Set[Collector[_<:DataType]]]


  // Policy properties (mutable)
  val properties = scala.collection.mutable.Set[Property[_]]()
  properties += new Property[String]("name", name) //Add name as a property


  def readProperty(s:String) = properties.find(_.name.equalsIgnoreCase(s)) match {
    case Some(p) => p.value
    case None => throw new NoSuchFieldException
  }

  /**
   * Graph representation
   * @return A Graph representation of this workflow
   */
  def graph = ToGraph(this)

  def add(c:Concept):Policy = {
    c match {
      case n:DataIO[_] => addIO(n)
      case n:Operation[_, _] => addActivity(n)
      case _ => throw new Exception(c + " is not handled by method add")
    }
  }

  def addIO(o:DataIO[_<:DataType]):Policy = {
    new Policy(name, ios + o, operations, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity[T<:DataType, O<:DataType](c:Operation[T,O]):Policy  = {
    c match {
      case Process(wf) => new Policy(name, ios, operations + c, links ++ autoConnectProcess(c.asInstanceOf[Process[_<:DataType, _<:DataType]]))
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
  def deleteIO(c:DataIO[_<:DataType]):Policy = {
    new Policy(name, ios - c, operations, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Return sub Workflow
   * @param root Root element
   */
  def subWorkflow(root:Concept, last:Option[Concept] = None):Policy = {
    val ios = new ArrayBuffer[DataIO[_<:DataType]]()
    val activities = new ArrayBuffer[Operation[_<:DataType, _<:DataType]]()
    val links = new ArrayBuffer[Link[_<:DataType]]()
    // Add root into the activities/ios

    root match {
      case elem:DataIO[_] => ios += elem
      case elem:Operation[_, _] => activities += elem
    }

    def internal(e:Concept):Unit = {
      val next = nextElements(e)
      next.foreach(e => e._1 match {
        case elem:Collector[_]  => ios += elem; links += e._2
        case elem:Sensor[_] =>  ios += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
        case elem:Operation[_, _] => activities += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
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

  def partition(selected:Set[Concept]) = {
    val pairs = for(x <- selected; y <- selected) yield (x,y)
    val partition = (selected, links.filter(p => pairs.contains((p.source, p.destination))))
    new Policy("partition", partition._1 collect {case x:DataIO[_] => x}, partition._1 collect {case x:Operation[_,_] => x}, partition._2)
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


  def linksTo(a:Concept) = links.filter(l => l.destination == a)

  def linksFrom(a:Concept) = links.filter(l => l.source == a)

  def +(w:Policy):Policy = new Policy(this.name + "_" + w.name, this.ios ++ w.ios, this.operations ++ w.operations, this.links ++ w.links)

  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + operations + "};links={" + links + "}]"

}

class Property[T](val name:String, val value:T)