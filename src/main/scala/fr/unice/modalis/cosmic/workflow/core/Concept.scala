package fr.unice.modalis.cosmic.workflow.core

/**
 * Represent a concept
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Concept {

  // Unique identifier
  val id:String = scala.util.Random.alphanumeric.take(5).mkString

  var isExtendable = true

  override def equals(x:Any) = x.isInstanceOf[Concept] && x.asInstanceOf[Concept].id == id

  def setExtendable(s: Boolean) = isExtendable = s // If set to true, the concept can an input or an output of the policy

}
