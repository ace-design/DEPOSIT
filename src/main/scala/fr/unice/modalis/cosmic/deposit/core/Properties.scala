package fr.unice.modalis.cosmic.deposit.core

/**
 * Properties trait
 * Handle properties
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */
trait Properties {


  val properties = scala.collection.mutable.Set[Property[_]]()

  def addProperty[T](name:String, value:T) = properties += new Property[T](name, value)

  def readProperty(s:String) = properties.find(_.name.equalsIgnoreCase(s)) match {
    case Some(p) => Some(p.value)
    case None => None
  }

  def hasProperty(s:String) = properties.find(_.name equalsIgnoreCase s)

}
class Property[T](val name:String, val value:T) {
  override def toString:String = name + "-->" + value
}
