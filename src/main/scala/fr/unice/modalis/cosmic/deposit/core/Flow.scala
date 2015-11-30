package fr.unice.modalis.cosmic.deposit.core

/**
 * An oriented flow between two concepts
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param source_output Flow source (component output)
 * @param destination_input Flow destination (component input)
 */
case class Flow[T<:DataType](source_output:Output[T], destination_input:Input[T])
{

  val source = source_output.parent
  val destination = destination_input.parent

  override def toString:String = source_output + "-->" + destination_input

  override def equals(a:Any):Boolean = a match {
    case Flow(src, dest)  => (src == source_output) && (dest == destination_input)
    case _ => false
  }

}