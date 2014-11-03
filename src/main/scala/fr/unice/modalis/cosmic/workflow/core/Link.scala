package fr.unice.modalis.cosmic.workflow.core

/**
 * An oriented link between two Workflow element
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param source Link source (component output)
 * @param destination Link destination (component input)
 */
case class Link[T <: DataType](val source:Output[T], val destination:Input[T])
{
  override def toString:String = source + "-->" + destination
}


