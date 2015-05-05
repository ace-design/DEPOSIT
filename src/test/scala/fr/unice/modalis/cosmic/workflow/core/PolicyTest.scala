package fr.unice.modalis.cosmic.workflow.core

import org.specs2.mutable.{SpecificationWithJUnit, Specification}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
class PolicyTest extends SpecificationWithJUnit {

  "Policy#name" should {
    "return the name of the policy" in { DCPTest.dcpA.name must beEqualTo("DCPA")}
  }

  "Properties" should {
    val testPolicy = DCPTest.dcpA
    "can be add in a policy" in {

      testPolicy.properties += new Property[String]("foo", "bar")
      testPolicy.readProperty("foo") must_== "bar"
    }

    "can be readen" in {
      testPolicy.readProperty("name") must_== testPolicy.name
    }

    "throw exception when not found" in {
      testPolicy.readProperty("foo2") must throwA[NoSuchFieldException]
    }
  }

  "Adding methods in a Policy" should {
    "add an input in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test")
      DCPTest.dcpA.addIO(s).ios must contain(s)
    }

    "add a concept in the policy" in {
      val a = Sub[IntegerType](Set())
      DCPTest.dcpA.addActivity(a).operations must contain(a)
    }

    "add an input with generic add in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test")
      DCPTest.dcpA.add(s).ios must contain(s)
    }

    "add an input with generic add in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test")
      DCPTest.dcpA.add(s).ios must contain(s)
    }

    "add an concept with generic add in the policy" in {
      val a = Sub[IntegerType](Set())
      DCPTest.dcpA.add(a).operations must contain(a)
    }

    "add a link in the policy" in {
      val l = Link[IntegerType](Constant[IntegerType](new IntegerType(3)).output, DCPTest.collectorA.input)
      DCPTest.dcpA.addLink(l).links must contain(l)
    }

  }

  "Deleting methods in a Policy" should {
    "delete an output in the policy" in {
      DCPTest.dcpA.deleteIO(DCPTest.collectorA).ios must not contain DCPTest.collectorA
    }

    "delete a concept in the policy" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).operations must not contain DCPTest.adder
    }

    "delete a link in the policy" in {
      DCPTest.dcpA.deleteLink(DCPTest.l).links must not contain DCPTest.l
    }

    "delete links to a concept if this latter is deleted" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).links.filter(l => l.source.equals(DCPTest.adder) || l.destination.equals(DCPTest.adder)) must be empty
    }
  }



  "Operators" should {
    "allow the partial reuse (select)" in {
      val selectedSources = DCPTest.dcpA.ios.filter {case p:Sensor[_] => p.url.contains("Hernan"); case _ => false}.asInstanceOf[Set[EventSensor[SantanderParkingType]]]
      val selectedProcesses = for (s <- selectedSources) yield DCPTest.dcpA.nextElements(s).head._1
      val selected = selectedSources ++ selectedProcesses ++ Set(DCPTest.adder)
      val result = DCPTest.dcpA.select(selected)

      "only selected concepts are present" in {
        (result.ios ++ result.operations) -- selected must be empty
      }

      "only links corresponding to selected concepts are present" in {
        result.links.forall(l => selected.contains(l.source) || selected.contains(l.destination)) must beTrue
      }



    }
    "allow the reuse (process)" in {
      val process = Process[SantanderParkingType, IntegerType](DCPTest.convert_workflow)
      "the inner workflow must be equal to the reused workflow" in {
        process.workflow must beEqualTo(DCPTest.convert_workflow)
      }
    }
    "weave a policy" in {
      pending
    }
  }

  "A Process" should {
    "have the same number of inputs as sensors" in {
      val process = Process[SantanderParkingType, IntegerType](DCPTest.convert_workflow)
      process.inputs must haveSize(DCPTest.convert_workflow.ios.collect({case n:Sensor[_] => n}).size)
    }

    "have the same number of outputs as collectors" in {
      val process = Process[SantanderParkingType, IntegerType](DCPTest.convert_workflow)
      process.outputs must haveSize(DCPTest.convert_workflow.ios.collect({case n:Collector[_] => n}).size)
    }
  }


}
