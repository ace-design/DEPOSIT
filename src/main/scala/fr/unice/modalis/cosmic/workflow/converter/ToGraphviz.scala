package fr.unice.modalis.cosmic.workflow.converter

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Graphviz generator. Useful for debug purposes
 * Created by Cyril Cecchinel - I3S Laboratory on 24/11/14.
 */
object ToGraphviz {

  def apply[T<:DataType](w: Workflow[T]): String = generateCode(w)

  def generateCode[T<:DataType](w: Workflow[T]): String = {
    val s = new StringBuilder
    s.append(generateHeader)
    s.append(generateNodeShape("box"))
    w.elements.foreach(e => s.append(generateElementCode(e) + ";\n"))
    w.links.foreach(l => s.append(generateLinkCode(l)))
    s.append(generateFooter())
    s.toString()
  }

  def generateNodeShape(shape: String) = "node [shape = " + shape + "]; \n"

  def generateHeader() = {
    "digraph finite_state_machine { rankdir=TB; size=\"12\"\n"
  }
  def generateLinkCode[T<:DataType](t: WFLink[T]) = {

    t.source.id + "->" + t.destination.id + (t.source match {
      case Predicate(_) => printlabel(t.source_output.name)
      case _ => ""
    }) + "\n"
  }


  def generateElementCode[T<:DataType](n: WFElement[T]) = {
    n.id + printlabel(n.toString)
  }

  def generateFooter() = "}"

  def printlabel(str:String) = "[label=\"" + str + "\"]"
}
