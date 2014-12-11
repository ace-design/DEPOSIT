package fr.unice.modalis.cosmic.workflow.core

/**
 * An oriented link between two Workflow element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param source_output Link source (component output)
 * @param destination_input Link destination (component input)
 */
case class WFLink(source_output:Output[_<:DataType], destination_input:Input[_<:DataType])
{

  val source = source_output.parent
  val destination = destination_input.parent

  override def toString:String = source + "-->" + destination

  override def equals(a:Any):Boolean = a match {
    case WFLink(a, b) if (a == source_output) && (b == destination_input) => true
    case _ => false
  }

}