package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.Verify
import fr.unice.modalis.cosmic.workflow.core.WFElement

import scala.collection.mutable.ArrayBuffer


/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param activities Workflow element list
 * @param links Link list

 */
case class Workflow(val name:String, val ios:Set[DataIO[_<:DataType]], val activities:Set[WFActivity[_<:DataType,_<:DataType]], val links:Set[WFLink[_<:DataType]]) {

  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  lazy val sources = ios.filter(_.isInstanceOf[Sensor[_<:DataType]]).asInstanceOf[Set[Sensor[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[Collector[_<:DataType]]).asInstanceOf[Set[Collector[_<:DataType]]]

  def addIO(o:DataIO[_<:DataType]):Workflow = {
    new Workflow(name, ios + o, activities, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity(c:WFActivity[_<:DataType,_<:DataType]):Workflow  = {
    c match {
      case Process(wf) => new Workflow(name, ios, activities + c, links ++ autoConnectProcess(c.asInstanceOf[Process[_<:DataType, _<:DataType]]))
      case _ => new Workflow(name, ios, activities + c, links)
    }

  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink[_<:DataType]):Workflow  = {
     new Workflow(name, ios, activities, links + l)
  }

  /**
   * Delete an activity in the current workflow (/!\ Delete also all links referring this activity)
   * @param c Workflow activity
   * @return A workflow without this activity and links referring this activity
   */
  def deleteActivity(c:WFActivity[_<:DataType,_<:DataType]):Workflow = {
    new Workflow(name, ios, activities - c, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Delete an IO in the current workflow (/!\ Delete also all links referring this IO)
   * @param c Workflow IO
   * @return A workflow without this IO and links referring this IO
   */
  def deleteIO(c:DataIO[_<:DataType]):Workflow = {
    new Workflow(name, ios - c, activities, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Return sub Workflow
   * @param root Root element
   */
  def subWorkflow(root:WFElement, last:Option[WFElement] = None):Workflow = {
    val ios = new ArrayBuffer[DataIO[_<:DataType]]()
    val activities = new ArrayBuffer[WFActivity[_<:DataType, _<:DataType]]()
    val links = new ArrayBuffer[WFLink[_<:DataType]]()
    // Add root into the activities/ios

    root match {
      case elem:DataIO[DataType] => ios += elem
      case elem:WFActivity[DataType, DataType] => activities += elem
    }

    def internal(e:WFElement):Unit = {
      val next = nextElements(e)
      next.foreach(e => e._1 match {
        case elem:Collector[DataType]  => ios += elem; links += e._2
        case elem:Sensor[DataType] =>  ios += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
        case elem:WFActivity[DataType, DataType] => activities += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
      }
      )
    }
    internal(root)
    new Workflow("sub" + name, ios.toSet, activities.toSet, links.toSet)
  }

  /**
   * Find the next workflow elements
   * @param e Current workflow element
   * @return Immediate next elements
   */
  def nextElements(e:WFElement):Set[(WFElement, WFLink[_<:DataType])] = {
    var res = Set[(WFElement, WFLink[_<:DataType])]()
    for (l <- links) {
      if (l.source == e)
        res = res ++ links.filter(l => l.source == e).foldLeft(Set.empty[(WFElement, WFLink[_<:DataType])]){(acc, e) => acc.+((e.destination, l))}
    }
    res
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink[_<:DataType]):Workflow  = {
    println("Delete [" + l + "]")
    new Workflow(name, ios, activities, links - l)
  }

  /**
   * Generate WFLinks to connect a process with current known sensors in a workflow
   * @param p Process
   * @tparam I Input Data type
   * @tparam O Output Data type
   * @return A set ok links needed to connect the process with the current known sensors
   */
  private def autoConnectProcess[I<:DataType, O<:DataType](p:Process[I,O]) = {
    var links = new ArrayBuffer[WFLink[_<:DataType]]()
    p.inputsNames.foreach(i => {
      val possibleSensor = ios.filter(p => p.isInstanceOf[Sensor[I]]).find(s => s.asInstanceOf[Sensor[I]].url == i)
      possibleSensor match {
        case Some(n) => links += new WFLink(n.asInstanceOf[Sensor[I]].output, p.getInput(i))
        case None => /* Nop */

      }
    }
    )
    links.toSet
  }

  def partition(selected:Set[WFElement]) = {
    val pairs = for(x <- selected; y <- selected) yield (x,y)
    val partition = (selected, links.filter(p => pairs.contains((p.source, p.destination))))
    val intermediateWF = new Workflow("partition", partition._1 collect {case x:DataIO[_] => x}, partition._1 collect {case x:WFActivity[_,_] => x}, partition._2)

    intermediateWF


  }

  val allInputs = {
    val array = ArrayBuffer[Input[_]]()
    ios.collect{case x:Collector[_] => x}.foreach(c => array += c.input)
    activities.foreach(a => array ++= a.inputs)
    array.toSet
  }

  val allOutputs = {
    val array = ArrayBuffer[Output[_]]()
    ios.collect{case x:Sensor[_] => x}.foreach(c => array += c.output)
    activities.foreach(a => array ++= a.outputs)
    array.toSet
  }

  def +(w:Workflow):Workflow = new Workflow(this.name + "_" + w.name, this.ios ++ w.ios, this.activities ++ w.activities, this.links ++ w.links)

  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + activities + "};links={" + links + "}]"

}
