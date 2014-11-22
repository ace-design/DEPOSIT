package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.actions.guard.constraint.ValueConstraint
import fr.unice.modalis.cosmic.workflow.algo.exception.NonMergeableException
import fr.unice.modalis.cosmic.workflow.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 13/11/14.
 */
class ComponentTest extends SpecificationWithJUnit{

  /* MERGING TESTS */
  val s1 = new Source[IntegerType]("TEMP")
  val s2 = new Source[IntegerType]("TEMP2")

  val g1 = new PeriodicGetter[IntegerType](10)
  val g2 = new PeriodicGetter[IntegerType](10)

  val p1 = new Predicate[IntegerType](new ValueConstraint[IntegerType]("<", 12))
  val p2 = new Predicate[IntegerType](new ValueConstraint[IntegerType]("<", 10))

  val c1 = new Sink[IntegerType]("alice")
  val c2 = new Sink[IntegerType]("bob")


  "Different elements are not equal" in {
    p1 must_!=  p2
  }

  "Same elements are equals" in {
    g1 must_== g2
  }

  "Merging two different sources must raise an exception" in {
    (s1 + s2) must throwA[NonMergeableException]

  }

  "Merging identical elements must produce the same element" in {
    (g1 + g2) must beEqualTo(g1)
  }


}
