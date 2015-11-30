package fr.unice.modalis.cosmic.deposit.converter

import fr.unice.modalis.cosmic.deployment.generator.CodeGenerator
import fr.unice.modalis.cosmic.deposit.core._

/**
 * Graphviz generator. Useful for debug purposes
 * Created by Cyril Cecchinel - I3S Laboratory on 24/11/14.
 */
object ToGraphviz {

  var showStubs = true

  def generateIO(io:PolicyIO[_<:DataType]) = {
    io match {
      case Collector(n, _) => generateNodeShape("triangle", "crimson") + io.id + printlabel(n) + ";"
      case PeriodicSensor(_,n, _) => generateNodeShape("invtriangle", "blue") + io.id + printlabel(n) + ";"
      case EventSensor(n, _) => generateNodeShape("invtriangle", "green") + io.id + printlabel(n) + ";"
      case a:JoinPointInput[_] if showStubs => generateNodeShape("doublecircle", "green") + io.id + (if(a.hasProperty("network").isDefined)  printlabel("\\[" + a.readProperty("network").get + "\\]")) +  ";"
      case a:JoinPointOutput[_] if showStubs =>  generateNodeShape("doublecircle", "crimson") + io.id + (if(a.hasProperty("network").isDefined)  printlabel("\\[" + a.readProperty("network").get + "\\]")) + ";"
      case GenericInput(name, _) => generateNodeShape("invtriangle", "darkslateblue") + io.id + printlabel(name) + ";"
      case GenericOutput(name, _) => generateNodeShape("triangle", "darkslateblue") + io.id + printlabel(name) + ";"
      case _ => ""
    }
  }

  def apply(w: Policy): String = generateCode(w)

  def writeSource(p:Policy):Unit = CodeGenerator.produceSourceFile(p.name , "graphviz", "dot", generateCode(p))

  def generateCode(w: Policy): String = {
    val s = new StringBuilder
    s.append(generateHeader)

    w.ios.foreach(e => s.append(generateIO(e) + "\n"))

    w.operations.foreach(e => s.append(generateElementCode(e) + ";\n"))
    w.flows.foreach(l => s.append(generateFlowCode(l)))
    s.append(generateFooter())
    s.toString()
  }

  def generateHeader = {
    "digraph finite_state_machine { rankdir=TB; size=\"20\"\n"
  }

 def generateNodeShape(shape: String):String = generateNodeShape(shape, "aquamarine")

  def generateNodeShape(shape: String, color:String):String = "node [shape=" + shape + ",color=" + color + ",style = filled]; \n"


  def generateFlowCode(t: Flow[_<:DataType]) = {
      t.source_output.parent.id + "->" + t.destination_input.parent.id + printlabel("o:" + t.source_output.name + " i:" + t.destination_input.name) + "\n"
  }

  def generateElementCode(n: Concept) = {
    (n match {
      case n:Process[_,_] => generateNodeShape("doubleoctagon", "gold")
      case _ => generateNodeShape("box")
    }) +
    n.id + printlabel(n.commonName)
  }

  def generateFooter() = "}"

  def printlabel(str:String) = "[label=\"" + str + "\"]"

}
