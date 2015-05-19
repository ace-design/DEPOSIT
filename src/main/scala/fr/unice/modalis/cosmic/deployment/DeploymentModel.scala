package fr.unice.modalis.cosmic.deployment

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{Inventory, NetworkTopology}
import fr.unice.modalis.cosmic.deposit.core.{DataType, Sensor, Concept, Policy}



/**
 * Created by Cyril Cecchinel - I3S Laboratory on 12/05/15.
 */
class DeploymentModel(val platform:String, val p:Policy)

object ToGraph {
  import scalax.collection.Graph
  import scalax.collection.GraphPredef._
  def apply(n: NetworkTopology with Inventory) = {
    val nodes = n.resources
    val edges = n.edges.map(l => l.source ~> l.destination)

    Graph.from(nodes, edges)
  }
}
object Deploy {

  /**
   * Obtain the list of sensors involved in a concept
   * @param c Concept
   * @param p Policy
   * @return List of sensors involved in a concept
   */
  def sensorsInvolved(c: Concept, p: Policy) = {

    var visited = List[Concept]()
    def inner(c:Concept):List[Sensor[_<:DataType]] = {
      c match {
        case n:Sensor[_] => List(n)
        case n:Concept => p.linksTo(n).map(_.source).foldLeft(List[Sensor[_<:DataType]]()){(acc, c) => if (!visited.contains(c)) {visited = c :: visited; inner(c) ::: acc} else acc}

      }
    }
    inner(c)

  }

  def prepare(p:Policy) = {
    p.operations.foreach(o => o.addProperty("sensors", sensorsInvolved(o, p)))
  }

}
