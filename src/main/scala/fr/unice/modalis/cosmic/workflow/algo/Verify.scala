package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Verify methods useful (in)-validate a workflow
 */
object Verify {

  /**
   * Get disconnected outputs in a workflow
   * @param wf Workflow
   * @return A set of all disconnected outputs
   */
 def getDisconnectedOutputs(wf:Policy) = wf.allOutputs -- wf.links.map(_.source_output)

  /**
   * Get disconnected inputs in a workflow
   * @param wf Workflow
   * @return A set of all disconnected inputs
   */
  def getDisconnectedInputs(wf:Policy) = wf.allInputs -- wf.links.map(_.destination_input)


}
