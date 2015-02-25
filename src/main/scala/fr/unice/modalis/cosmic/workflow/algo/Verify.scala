package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 24/02/15.
 */
object Verify {


 def getNotConnectedOutputs(wf:Workflow) = wf.allOutputs -- wf.links.map(_.source_output)

  def getNotConnectedInputs(wf:Workflow) = wf.allInputs -- wf.links.map(_.destination_input)


}
