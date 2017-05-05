package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.deployment.network.Entity

/**
 * Represent a concept
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Concept extends Properties{

  // Unique identifier
  def id:String = scala.util.Random.alphanumeric.take(5).mkString

  // Concept common name
  val commonName:String

  // Define if the concept can host join points
  private var _isExpendable = true

  /**
   * Check the equality between instances
   * @param x An object instance
   * @return Equal?
   */
  override def equals(x:Any) = x.isInstanceOf[Concept] && x.asInstanceOf[Concept].id == id

  /**
   * Set if the concept can host join points
   * @param s Hosting parameter
   */
  def setExpendable(s: Boolean) = _isExpendable = s // If set to true, the concept can an input or an output of the policy

  /**
   * Return if the concept can host join points
   * @return Hosting parameter
   */
  def isExpendable = _isExpendable

  /**
   * Return a copy of this concept (with different id)
   * @return copy of this concept
   */
  def duplicate:Concept

  /**
    * A boolean equation that a concept must satisfy in order to be placed on an entity (Default=true)
    * @param e Entity
    * @return A boolean indicating if the concept can be placed on the entity e
    */
  def placingConstraintsEquation(e:Entity):Boolean = true

  var _marker:Option[String] = None
  def setMarker(s:String) = _marker = Some(s)
  def cleanMarker() = _marker = None
}
