package fr.unice.modalis.cosmic.workflow

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.algo.Verify
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
class VerifyTest extends SpecificationWithJUnit{


  "A user workflow has to be connected" in {
    Verify.connectivity(GenericScenarios.easy) must beTrue
  }
  "A user workflow has to be connected (2)" in {
    Verify.connectivity(GenericScenarios.twoSources) must beTrue
  }
  "A user workflow has to be connected (3)" in {
    Verify.connectivity(GenericScenarios.error1) must beFalse
  }

  "A workflow must have sinks" in {
    Verify.collectorSink(GenericScenarios.twoSources) must beTrue
  }
  "A workflow must have sinks (2)" in {
    Verify.collectorSink(GenericScenarios.noSink) must beFalse
  }
}
