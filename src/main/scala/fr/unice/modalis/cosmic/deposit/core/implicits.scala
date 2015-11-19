package fr.unice.modalis.cosmic.deposit.core
import scala.language.implicitConversions
/**
  * Created by Cyril Cecchinel - I3S Laboratory on 19/11/2015.
  */
trait Implicits {



  var currentConcept: Concept = _
  var conceptsSet: Set[Concept] = Set()
  var currentFlow : Link[_<:DataType] = _
  var flowsSet : Set[Link[_<:DataType]] = Set()

  def saveCurrentConcepts() = {
    if (currentConcept != null)
      conceptsSet += currentConcept
    currentConcept = null
  }

  def concepts(contents: => Unit): Set[Concept] = {
    contents
    saveCurrentConcepts()
    conceptsSet
  }

  implicit def conceptBuilder(c:Concept):Concept = {
    saveCurrentConcepts()
    c
  }


  def saveCurrentFlows(): Unit = {
    if (currentFlow != null)
      flowsSet += currentFlow
    currentFlow = null
  }

  def flows(contents: => Unit): Set[Link[_<:DataType]] = {
    contents
    saveCurrentFlows()
    flowsSet
  }

  implicit def linkBuilder[T<:DataType](o:Output[T]):LinkBuilder[T] = {
    saveCurrentFlows()
    LinkBuilder(o)
  }

  protected case class LinkBuilder[T<:DataType](src:Output[T]) {
    implicit def ->(dst:Input[T]):Link[T] = {
      val updated = Link(src, dst)
      currentFlow = updated
      updated
    }
  }

}
