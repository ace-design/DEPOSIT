package fr.unice.modalis.cosmic.deployment.network.dsl.kernel

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.Media.Media
import fr.unice.modalis.cosmic.deposit.core.Properties
/**
 * Represent sensing infrastructure nodes
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */

trait GenericNode extends Properties{
  def isProgrammable:Boolean = readProperty("programmable").getOrElse(false).asInstanceOf[Boolean]
  val name:String
}

case class Node(name:String) extends GenericNode
case class Sensor(name:String) extends GenericNode
case class Bridge(name:String) extends GenericNode
case class SensorPlatform(name:String) extends GenericNode
case class Remote(name: String) extends GenericNode
case class Repeater(name:String) extends GenericNode
case class Edge(source: GenericNode, destination:GenericNode, media:Media = Media.Unknown)

object Media extends Enumeration {
  type Media = Value
  val XBee, Serial, ZWave, USB, Unknown = Value
}
