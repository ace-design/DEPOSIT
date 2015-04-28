package fr.unice.modalis.cosmic.workflow.core

import org.specs2.mutable.{SpecificationWithJUnit, Specification}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
class WorkflowTest extends SpecificationWithJUnit {

  "Policy#name" should {
    "return the name of the policy" in { DCPTest.dcpA.name must beEqualTo("DCPA")}

  }

  "Adding methods in a Policy" should {
    "add an input in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test")
      DCPTest.dcpA.addIO(s).ios must contain(s)
    }

    "add a concept in the policy" in {
      val a = Sub[IntegerType](Set())
      DCPTest.dcpA.addActivity(a).activities must contain(a)
    }

    "add a link in the policy" in {
      val l = Link[IntegerType](Constant[IntegerType](new IntegerType(3)).output, DCPTest.collectorA.input)
      DCPTest.dcpA.addLink(l).links must contain(l)
    }

    "add join points to a concept" in {
      val a = Sub[IntegerType](Set("i1", "i2"))
      DCPTest.dcpA.addActivity(a).ios.count {case JointPointInput(e) if a == e => true; case JointPointOutput(e) if a == e => true; case _ => false} must_== 3
    }
  }

  "Deleting methods in a Policy" should {
    "delete an output in the policy" in {
      DCPTest.dcpA.deleteIO(DCPTest.collectorA).ios must not contain(DCPTest.collectorA)
    }

    "delete a concept in the policy" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).activities must not contain DCPTest.adder
    }

    "delete a link in the policy" in {
      DCPTest.dcpA.deleteLink(DCPTest.l).links must not contain DCPTest.l
    }

    "delete links to a concept if this latter is deleted" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).links.filter(l => (l.source.equals(DCPTest.adder)) || (l.destination.equals(DCPTest.adder))) must be empty
    }
  }



  "Operators" should {
    "allow the partial reuse (select)" in {
      pending
    }
    "allow the reuse (process)" in {
      pending
    }
    "weave a policy" in {
      pending
    }
  }


}
