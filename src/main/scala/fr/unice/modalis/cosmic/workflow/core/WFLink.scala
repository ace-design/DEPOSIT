package fr.unice.modalis.cosmic.workflow.core

/**
 * An oriented link between two Workflow element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param source_output Link source (component output)
 * @param destination_input Link destination (component input)
 */
case class WFLink[T <: DataType](source_output:Output[T], destination_input:Input[T])
{

  val source = source_output.parent
  val destination = destination_input.parent

  override def toString:String = source.toString + "-->" + destination.toString


}