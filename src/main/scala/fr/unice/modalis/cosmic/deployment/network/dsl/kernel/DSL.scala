package fr.unice.modalis.cosmic.deployment.network.dsl.kernel

import scalax.collection.Graph
import scalax.collection.GraphPredef._
/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */
trait NetworkTopology { inventory:Inventory =>
  import scala.language.implicitConversions

  var edges: Set[Edge]  = Set()

  var currentEdge: Edge  = _


  def declare (contents: => Unit): Unit = {
    contents
    saveCurrentTopology()
  }

  private def saveCurrentTopology(): Unit = {
    if (currentEdge != null)
      edges += currentEdge
    currentEdge = null
  }


  def isConnectedTo(n: Node) = edges.filter(_.source == n).map(_.destination)
  def isConnectedBy(n: Node) = edges.filter(_.destination == n).map(_.source)

  protected case class EdgeBuilder(fromId: String = "", toId:String = "", from:Node = null, to: Node = null) {
    val source = (inventory.resources find {r => r.name == fromId}).get
    def isConnectedTo(id:String):EdgeBuilder = {
      val target = (inventory.resources find {r => r.name == id}).get
      val updated = this.copy(from = source, to = target); currentEdge = updated; updated
    }
  }

  implicit def str2EdgeBuilder(s:String):EdgeBuilder = {
    saveCurrentTopology()
    EdgeBuilder(s)
  }
 implicit protected def edgeBuilderToEdge(builder: EdgeBuilder): Edge = {
   Edge(builder.from, builder.to)
 }

  def getSensorsFromNode(n: Node):Set[Sensor] = {
    var visited = List[Node]()
    def inner(n: Node):List[Sensor] = {
      n match {
        case n:Sensor => List(n)
        case _ => isConnectedBy(n).foldLeft(List[Sensor]()){(acc, n) => if (!visited.contains(n)) {visited = n :: visited; inner(n) ::: acc} else acc}
      }
    }
    inner(n).toSet
  }

  def toGraph = {
    val _nodes = inventory.resources
    val _edges = edges.map(l => l.source ~> l.destination)
    Graph.from(_nodes, _edges)
  }


}


trait Inventory {
  import scala.language.implicitConversions

  def deployed(contents: => Unit): Unit = {
    contents
    saveCurrentInventory()
  }

  protected def aSensor() = {
    saveCurrentInventory()
    NodeBuilder("sensor")
  }

  protected def aSensorPlatform() = {
    saveCurrentInventory()
    NodeBuilder("sensorPlatform")
  }

  protected def aRepeater(): Unit ={
    saveCurrentInventory()
    NodeBuilder("repeater")
  }

  protected def aBridge() = {
    saveCurrentInventory()
    NodeBuilder("bridge")
  }

  protected def aRemoteCollector() = {
    saveCurrentInventory()
    NodeBuilder("remote")
  }

  private def saveCurrentInventory() = {
    if (currentResource != null) {
      resources += currentResource
      currentResource = null
    }
  }

  var resources: Set[Node] = Set()
  private var currentResource: Node = _

  protected case class NodeBuilder(nodeType: String, id:String = "") {
    def withId(i: String): NodeBuilder = {val updated = this.copy(nodeType, id = i); currentResource = updated; updated}
  }

  implicit def NodeBuilder2Node(builder: NodeBuilder): Node = {
    builder.nodeType match {
      case "sensor" => Sensor(builder.id)
      case "sensorPlatform" => SensorPlatform(builder.id)
      case "bridge" => Bridge(builder.id)
      case "remote" => Remote(builder.id)
      case "repeater" => Repeater(builder.id)
    }
  }
}

