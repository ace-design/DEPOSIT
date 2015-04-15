package fr.unice.modalis.cosmic.workflow.core

import fr.unice.modalis.cosmic.workflow.algo.Verify
import fr.unice.modalis.cosmic.workflow.converter.ToGraph

import scala.collection.mutable.ArrayBuffer


/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param activities Workflow element list
 * @param links Link list

 */
case class Workflow(val name:String, val ios:Set[DataIO[_<:DataType]], val activities:Set[Activity[_<:DataType,_<:DataType]], val links:Set[Link[_<:DataType]]) {


  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  lazy val sources = ios.filter(_.isInstanceOf[Sensor[_<:DataType]]).asInstanceOf[Set[Sensor[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[Collector[_<:DataType]]).asInstanceOf[Set[Collector[_<:DataType]]]

  /**
   * Graph representation
   * @return A Graph representation of this workflow
   */
  def graph = ToGraph(this)

  def addIO(o:DataIO[_<:DataType]):Workflow = {
    new Workflow(name, ios + o, activities, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity[T<:DataType, O<:DataType](c:Activity[T,O]):Workflow  = {
    var newWF = new Workflow()
    c match {
      case Process(wf) => newWF = new Workflow(name, ios, activities + c, links ++ autoConnectProcess(c.asInstanceOf[Process[_<:DataType, _<:DataType]]))
      case _ => newWF = new Workflow(name, ios, activities + c, links)
    }

    // Add stubs for all disconnected inputs, outputs
    val linksToAdd = new ArrayBuffer[Link[_<:DataType]]()
    val iosToAdd = new ArrayBuffer[DataIO[_<:DataType]]()

    val dOutputs = Verify.getDisconnectedOutputs(newWF)
    val dInputs = Verify.getDisconnectedInputs(newWF)

    dOutputs.foreach(o => {
      val l = new Link(o, new StubOutput().input)
      val s = l.destination
      linksToAdd += l
      iosToAdd += s.asInstanceOf[StubOutput[_<:DataType]]
    })

    dInputs.foreach(i => {
      val l = new Link(new StubInput().output, i)
      val s = l.source
      linksToAdd += l
      iosToAdd += s.asInstanceOf[StubInput[_<:DataType]]
    })

    iosToAdd.foreach(o => newWF = newWF.addIO(o))
    linksToAdd.foreach(l => newWF = newWF.addLink(l))

    newWF
  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:Link[_<:DataType]):Workflow  = {
    // It can be a link between :

    (l.source, l.destination) match {
      // Output Stub -> Activity : Link between OutputStub predecessor and activity + delete the output stub
      //case (x:WorkflowStubOutput[_], y:WFActivity[_,_]) => this.addLink(new WFLink(x.predecessor, l.destination_input)).deleteIO(x)
      // Output Stub -> Input Stub : Link between OutputStub predecessor and InputStub successor + delete the output and input stubs
      //case (x:WorkflowStubOutput[_], y:WorkflowStubInput[_]) => this.addLink(new WFLink(x.predecessor, y.successor)).deleteIO(x).deleteIO(y)

      // Anything else : Activity -> Activity (nothing to do)
      case (_,_) => new Workflow(name, ios, activities, links + l)
    }

  }

  /**
   * Delete an activity in the current workflow (/!\ Delete also all links referring this activity)
   * @param c Workflow activity
   * @return A workflow without this activity and links referring this activity
   */
  def deleteActivity(c:Activity[_<:DataType,_<:DataType]):Workflow = {
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
  def subWorkflow(root:Element, last:Option[Element] = None):Workflow = {
    val ios = new ArrayBuffer[DataIO[_<:DataType]]()
    val activities = new ArrayBuffer[Activity[_<:DataType, _<:DataType]]()
    val links = new ArrayBuffer[Link[_<:DataType]]()
    // Add root into the activities/ios

    root match {
      case elem:DataIO[DataType] => ios += elem
      case elem:Activity[DataType, DataType] => activities += elem
    }

    def internal(e:Element):Unit = {
      val next = nextElements(e)
      next.foreach(e => e._1 match {
        case elem:Collector[DataType]  => ios += elem; links += e._2
        case elem:Sensor[DataType] =>  ios += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
        case elem:Activity[DataType, DataType] => activities += elem; links += e._2; if (e != Set.empty && !last.isDefined || last.get != e._1) internal(e._1)
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
  def nextElements(e:Element):Set[(Element, Link[_<:DataType])] = {
    var res = Set[(Element, Link[_<:DataType])]()
    for (l <- links) {
      if (l.source == e)
        res = res ++ links.filter(l => l.source == e).foldLeft(Set.empty[(Element, Link[_<:DataType])]){(acc, e) => acc.+((e.destination, l))}
    }
    res
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:Link[_<:DataType]):Workflow  = {
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

  def partition(selected:Set[Element]) = {
    val pairs = for(x <- selected; y <- selected) yield (x,y)
    val partition = (selected, links.filter(p => pairs.contains((p.source, p.destination))))
    var intermediateWF = new Workflow("partition", partition._1 collect {case x:DataIO[_] => x}, partition._1 collect {case x:Activity[_,_] => x}, partition._2)

    // Add stubs for all disconnected inputs, outputs
    val linksToAdd = new ArrayBuffer[Link[_<:DataType]]()
    val iosToAdd = new ArrayBuffer[DataIO[_<:DataType]]()

    val dOutputs = Verify.getDisconnectedOutputs(intermediateWF)
    val dInputs = Verify.getDisconnectedInputs(intermediateWF)

    dOutputs.foreach(o => {
     // val l = new WFLink(o, new WorkflowStubOutput(o).input)
     val l = new Link(o, new StubOutput().input)
      val s = l.destination
      linksToAdd += l
      iosToAdd += s.asInstanceOf[StubOutput[_<:DataType]]
    })

    dInputs.foreach(i => {
     // val l = new WFLink(new WorkflowStubInput(i).output, i)
     val l = new Link(new StubInput().output, i)
      val s = l.source
      linksToAdd += l
      iosToAdd += s.asInstanceOf[StubInput[_<:DataType]]
    })

    iosToAdd.foreach(o => intermediateWF = intermediateWF.addIO(o))
    linksToAdd.foreach(l => intermediateWF = intermediateWF.addLink(l))

    intermediateWF


  }

  def allInputs[T<:DataType] = {
    val array = ArrayBuffer[Input[_<:DataType]]()
    ios.collect{case x:Collector[T] => x}.foreach(c => array += c.input)
    activities.foreach(a => array ++= a.inputs)
    array.toSet
  }

  def allOutputs[T<:DataType] = {
    val array = ArrayBuffer[Output[_<:DataType]]()
    ios.collect{case x:Sensor[T] => x}.foreach(c => array += c.output)
    activities.foreach(a => array ++= a.outputs)
    array.toSet
  }


  def linksTo(a:Element) = links.filter(l => l.destination == a)

  def linksFrom(a:Element) = links.filter(l => l.source == a)

  def +(w:Workflow):Workflow = new Workflow(this.name + "_" + w.name, this.ios ++ w.ios, this.activities ++ w.activities, this.links ++ w.links)

  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + activities + "};links={" + links + "}]"

}
