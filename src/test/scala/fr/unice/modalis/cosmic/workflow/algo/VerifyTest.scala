package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
class VerifyTest extends SpecificationWithJUnit{

  //Links
  val link1 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new Predicate[IntegerType](ValueConstraint("==", 2)).input) //Invalid

  val link2 = new WFLink[IntegerType](new Source[IntegerType]("TEST").output, new PeriodicGetter[IntegerType](10000).input) //Valid

  "A valid link must pass checkLink" in {
    Verify.checkLink(link2) must beTrue
  }

  "A invalid link must failed" in {
    Verify.checkLink(link1) must beFalse
  }

}
