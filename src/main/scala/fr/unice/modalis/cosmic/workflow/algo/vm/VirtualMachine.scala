package fr.unice.modalis.cosmic.workflow.algo.vm

import fr.unice.modalis.cosmic.workflow.core.{DataType, Workflow}

/**
 * Compute new Workflows
 * Created by Cyril Cecchinel - I3S Laboratory on 04/11/14.
 */
object VirtualMachine {

  /**
   * Apply an action list on a workflow
   * @param w Current workflow
   * @param arr Action list
   * @tparam T Type of workflow
   * @return A new workflow with the actions list applied
   */
  def apply[T<:DataType](w: Workflow[T], arr:List[Instruction[T]]):Workflow[T] = arr match {
    case x :: tail => apply(x.make(w), tail)
    case Nil => w
  }
}
