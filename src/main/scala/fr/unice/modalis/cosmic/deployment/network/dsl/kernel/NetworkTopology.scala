package fr.unice.modalis.cosmic.deployment.network.dsl.kernel

import fr.unice.modalis.cosmic.deposit.core.Properties
/**
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */

trait GenericNode extends Properties{
  def isProgrammable:Boolean = readProperty("programmable").getOrElse(false).asInstanceOf[Boolean]
  val name:String
}

case class Node(val name:String) extends GenericNode
case class Sensor(val name:String) extends GenericNode
case class Bridge(val name:String) extends GenericNode
case class SensorPlatform(val name:String) extends GenericNode
case class Remote(val name: String) extends GenericNode
case class Repeater(val name:String) extends GenericNode
case class Edge(val source: GenericNode, val destination:GenericNode)

