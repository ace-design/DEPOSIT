package fr.unice.modalis.cosmic.deployment.generator

import scala.collection.mutable

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 05/10/15.
 */
trait CodeGenerator {
  def replace(parameter:String, value:String, source:String):String = source.replace("#@" + parameter + "@#", value)

  case class Variable(name:String, t:String)

  val functions = mutable.HashMap[String,String]()
  val variables = mutable.Set[Variable]()
}

case class NonHandledSensorException(io:Any) extends Exception("Non handled sensor " + io)

