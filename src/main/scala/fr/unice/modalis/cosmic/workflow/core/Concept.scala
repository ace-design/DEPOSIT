package fr.unice.modalis.cosmic.workflow.core

/**
 * Represent a concept
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait Concept {

  // Unique identifier
  val id:String = scala.util.Random.alphanumeric.take(5).mkString


  override def equals(x:Any) = x.isInstanceOf[Concept] && x.asInstanceOf[Concept].id == id

}
