package fr.unice.modalis.cosmic.deployment.network.dsl.kernel

import scalax.collection.Graph
import scalax.collection.GraphPredef._
/**
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */

trait Node
case class Sensor(val name:String) extends Node
case class Bridge(val name:String) extends Node
case class SensorPlatform(val name:String) extends Node
case class Remote(val url: String) extends Node

case class Edge(val source: Node, val destination:Node)

