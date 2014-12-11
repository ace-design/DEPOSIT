package fr.unice.modalis.cosmic.workflow

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
  val s3 = new Source[IntegerType]("TEMP")



  val c1 = new Sink[IntegerType]("alice")
  val c2 = new Sink[IntegerType]("bob")


  "Different elements are not equal" in {
    s1.equals(s2) must_==  false
  }

  "Same elements are equals" in {
    s3.equals(s3) must_== true
  }

  "Same elements with different references are similar" in {
    s1 ~ s3 must beTrue
  }

  "Different elements are not similar" in {
    s1 ~ s2 must beFalse
  }

  "The merge of two different sources must raise an exception" in {
    (s1 + s2) must throwA (new NonMergeableException)

  }

  "The merge of two different but equals sources must NOT raise an exception" in {
    (s1 + s3) must not throwA (new NonMergeableException)
  }


}
