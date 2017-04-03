package fr.unice.modalis.cosmic.deposit.core

import fr.unice.modalis.cosmic.ComprehensivePolicyWithoutDSL
import fr.unice.modalis.cosmic.deposit.algo.{Unification, Weave}
import fr.unice.modalis.cosmic.deposit.core.Policy.NonValidPolicyException
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Policy test
 * Created by Cyril Cecchinel - I3S Laboratory on 28/04/15.
 */
class PolicyTest extends SpecificationWithJUnit {

  "Validation of policies" should {
    "avoid loops" in {
      object testPolicy extends DEPOSIT{
        this handles classOf[SmartCampusType]
        val a = declare aPeriodicSensor() named "A" withPeriod 3
        val c = declare aCollector() named "C"
        val b = define anAdder() withInputs("i1", "i2")

        flows {
          a() -> b("i1")
          b("output") -> c()
          b("output") -> b("i2")
        }

        def innerPolicy() = this.policy
      }
      Policy.checkValidity(testPolicy.innerPolicy) must throwA[NonValidPolicyException].like {case e => e.getMessage === testPolicy.innerPolicy().name + " is not valid because has a loop on a concept"}
    }

  }
  "Duplication of policies" should {
    val a = DCPTest.dcpA
    val b = DCPTest.dcpA.duplicate

    "policies must not be equals" in {
      a mustNotEqual b
    }

    "policies must have the same amount of ios" in {
      b.ios must haveSize (a.ios.size)
    }

    "policies must have the same amount of operations" in {
      b.operations must haveSize (a.operations.size)
    }

    "policies must have the same amount of links" in {
      b.flows must haveSize (a.flows.size)

    }
  }
  "Policy accessors" should {
    "return the name of the policy" in { DCPTest.dcpA.name must beEqualTo("DCPA")}
    "return a sensor by its name" in {ComprehensivePolicyWithoutDSL.p.findSensorByName("AC_443") must beSome}
  }

  "A concept can be found by its id" in {
    DCPTest.p2.findConceptById(DCPTest.c2.id) must beSome(DCPTest.c2)
    DCPTest.p2.findConceptById("wrongid") must beNone
  }

  "Properties" should {
    val testPolicy = DCPTest.dcpA
    "can be add in a policy" in {

      testPolicy.properties += new Property[String]("foo", "bar")
      testPolicy.readProperty("foo") must beSome("bar")
    }


    "throw exception when not found" in {
      testPolicy.readProperty("foo2") must beNone
    }
  }

  "Adding methods in a Policy" should {
    "add an input in the policy" in {
      val s = PeriodicSensor(3, "test", classOf[IntegerType])
      DCPTest.dcpA.addIO(s).ios must contain(s)
    }

    "add a concept in the policy" in {
      val a = Sub(Set(), classOf[IntegerType])
      DCPTest.dcpA.addOperation(a).operations must contain(a)
    }

    "add an input with generic add in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test", classOf[IntegerType])
      DCPTest.dcpA.add(s).ios must contain(s)
    }

    "add an input with generic add in the policy" in {
      val s = PeriodicSensor[IntegerType](3, "test", classOf[IntegerType])
      DCPTest.dcpA.add(s).ios must contain(s)
    }

    "add an concept with generic add in the policy" in {
      val a = Sub(Set(), classOf[IntegerType])
      DCPTest.dcpA.add(a).operations must contain(a)
    }

    "add a link in the policy" in {
      val l = Flow(Constant(new IntegerType(3),  classOf[IntegerType]).output, DCPTest.collectorA.input)
      DCPTest.dcpA.addFlow(l).flows must contain(l)
      DCPTest.dcpA.add(l).flows must contain(l)

    }

  }

  "Deleting methods in a Policy" should {
    "delete an output in the policy" in {
      DCPTest.dcpA.deleteIO(DCPTest.collectorA).ios must not contain DCPTest.collectorA
      DCPTest.dcpA.delete(DCPTest.collectorA).ios must not contain DCPTest.collectorA

    }

    "delete a concept in the policy" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).operations must not contain DCPTest.adder
      DCPTest.dcpA.delete(DCPTest.adder).operations must not contain DCPTest.adder
    }

    "delete a link in the policy" in {
      DCPTest.dcpA.deleteFlow(DCPTest.l).flows must not contain DCPTest.l
      DCPTest.dcpA.delete(DCPTest.l).flows must not contain DCPTest.l

    }

    "delete links to a concept if this latter is deleted" in {
      DCPTest.dcpA.deleteActivity(DCPTest.adder).flows.filter(l => l.source.equals(DCPTest.adder) || l.destination.equals(DCPTest.adder)) must be empty
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
        result.flows.forall(l => selected.contains(l.source) || selected.contains(l.destination)) must beTrue
      }



    }
    "allow the reuse (process)" in {
      val process = Process(DCPTest.convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])
      "the inner workflow must be equal to the reused workflow" in {
        process.workflow must beEqualTo(DCPTest.convert_workflow)
      }
    }
    "weave a policy" in {
      "unify join points" in {
        val u1 = new Unification[IntegerType](
          DCPTest.p1e.outputJoinPoints.find(p => p.fromConceptOutput.name == DCPTest.a1.getOutput().name).get.asInstanceOf[JoinPointOutput[IntegerType]],
          DCPTest.p3e.inputJoinPoints.find(p => p.toConceptInput.name == DCPTest.add.getInput("i1").name).get.asInstanceOf[JoinPointInput[IntegerType]])
        Weave(DCPTest.p1e, DCPTest.p3e, Set(u1)).flows.find(f => f.source_output == DCPTest.a1.output && f.destination_input == DCPTest.add.getInput("i1")) must be some
      }
    }
  }

  "A Process" should {
    "have the same number of inputs as sensors" in {
      val process = Process(DCPTest.convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])
      process.inputs must haveSize(DCPTest.convert_workflow.ios.collect({case n:Sensor[_] => n}).size)
    }

    "have the same number of outputs as collectors" in {
      val process = Process(DCPTest.convert_workflow, classOf[SantanderParkingType], classOf[IntegerType])
      process.outputs must haveSize(DCPTest.convert_workflow.ios.collect({case n:Collector[_] => n}).size)
    }
  }

  "A sub policy" should {
    "Compute the sub-policy between a root and a defined leaf" in {
      val root = ComprehensivePolicyWithoutDSL.door_filter
      val leaf = ComprehensivePolicyWithoutDSL.collector

      val subPolicy = ComprehensivePolicyWithoutDSL.p.subPolicy(root, leaf)

      subPolicy.concepts must haveLength(3)
      subPolicy.flows must haveLength(2)
    }

    "Compute the sub-policy between a root and all collectors" in {
      val root = ComprehensivePolicyWithoutDSL.ac443

      val subPolicy = ComprehensivePolicyWithoutDSL.p.subPolicy(root)

      subPolicy.concepts must haveLength(6)
      subPolicy.flows must haveLength(5)
    }

    "retrieve the flows between two immediate concepts" in {
      val source = ComprehensivePolicyWithoutDSL.ac443
      val destination1 = ComprehensivePolicyWithoutDSL.temp_filter
      val destination2 = ComprehensivePolicyWithoutDSL.collector

      val policy = ComprehensivePolicyWithoutDSL.p

      policy.getFlowsBetween(source, destination1) must haveLength(1)
      policy.getFlowsBetween(source, destination2) must haveLength(0)


    }

  }

}
