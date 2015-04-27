package fr.unice.modalis.cosmic.workflow.converter

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Graphviz generator. Useful for debug purposes
 * Created by Cyril Cecchinel - I3S Laboratory on 24/11/14.
 */
object ToGraphviz {

  var showStubs = true

  def generateIO(io:DataIO[_<:DataType]) = {
    io match {
      case Collector(n) => generateNodeShape("triangle", "crimson") + io.id + printlabel(n) + ";"
      case PeriodicSensor(_,n) => generateNodeShape("invtriangle", "blue") + io.id + printlabel(n) + ";"
      case EventSensor(n) => generateNodeShape("invtriangle", "green") + io.id + printlabel(n) + ";"
      case JointPointInput() if showStubs => generateNodeShape("doublecircle", "green") + io.id + ";"
      case JointPointOutput() if showStubs =>  generateNodeShape("doublecircle", "crimson") + io.id + ";"
      case _ => ""
    }
  }

def apply(w: Workflow): String = generateCode(w)

  def generateCode(w: Workflow): String = {
    val s = new StringBuilder
    s.append(generateHeader)

    w.ios.foreach(e => s.append(generateIO(e) + "\n"))

    w.activities.foreach(e => s.append(generateElementCode(e) + ";\n"))
    w.links.foreach(l => s.append(generateLinkCode(l)))
    s.append(generateFooter())
    s.toString()
  }

  def generateHeader() = {
    "digraph finite_state_machine { rankdir=TB; size=\"20\"\n"
  }

 def generateNodeShape(shape: String):String = generateNodeShape(shape, "aquamarine")

  def generateNodeShape(shape: String, color:String):String = "node [shape=" + shape + ",color=" + color + ",style = filled]; \n"


  def generateLinkCode(t: Link[_<:DataType]) = {
//    if (t.source.isInstanceOf[Stub[_]] || t.destination.isInstanceOf[Stub[_]] && !showStubs)
//      ""
//    else
      t.source_output.parent.id + "->" + t.destination_input.parent.id + printlabel("o:" + t.source_output.name + " i:" + t.destination_input.name) + "\n"
  }

  def generateElementCode(n: Element) = {
    (n match {
      case Process(_) => generateNodeShape("doubleoctagon", "gold")
      case _ => generateNodeShape("box")
    }) +
    n.id + printlabel(n.toString)
  }

  def generateFooter() = "}"

  def printlabel(str:String) = "[label=\"" + str + "\"]"

}
