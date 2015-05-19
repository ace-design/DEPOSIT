package fr.unice.modalis.cosmic.deployment.network.dsl.kernel

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */
trait NetworkTopology {

}


trait Inventory {
  import scala.language.implicitConversions

  def declare(contents: => Unit): Unit = {
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
    }
  }
}

