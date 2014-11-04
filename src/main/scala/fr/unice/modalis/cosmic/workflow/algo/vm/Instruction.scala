package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{DataType, Workflow}

/**
 * Instruction trait
 * Represent actions performed by the virtual machine
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
trait Instruction[T<:DataType] {

  /**
   * Apply the action on a workflow
   * @param w Workflow
   * @return A new workflow
   */
  def make(w: Workflow[T]): Workflow[T]

  override def toString:String

}
