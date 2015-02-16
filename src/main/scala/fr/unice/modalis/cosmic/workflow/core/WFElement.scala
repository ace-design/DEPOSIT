package fr.unice.modalis.cosmic.workflow.core

/**
 * Represent a WorkFlow Element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
trait WFElement {

  // Unique identifier
  var id:String = scala.util.Random.alphanumeric.take(5).mkString


  override def equals(x:Any) = x.isInstanceOf[WFElement] && x.asInstanceOf[WFElement].id == id

}
