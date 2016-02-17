import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorBrand.SensorBrand
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorType.SensorType
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deployment.infrastructure.InfrastructureModel
import fr.unice.modalis.cosmic.deployment.network.Entity
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Decompose, Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core.Policy.NonValidPolicyException
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.{ComprehensivePolicy, ComprehensivePolicyWithoutDSL, NonValidPolicy, NonValidPolicy2}
import org.specs2.mutable.SpecificationWithJUnit

/**
  * This is a specification test of a data collection policy
  * defined with the DEPOSIT DSL
  * Created by Cyril Cecchinel - I3S Laboratory on 20/01/2016.
  */


class SpecificationsTest extends SpecificationWithJUnit{
  val demo_policy = ComprehensivePolicy.innerPolicy()

  "A non valid data collection policy defined with DEPOSIT" should {
    "fail the validity check test (1)" in {
      Policy.checkValidity(NonValidPolicy.innerPolicy()) must throwA[NonValidPolicyException].like {case e => e.getMessage === NonValidPolicy.innerPolicy().name + " is not valid because has an input port has more than one incoming data flow"}
    }

    "fail the validity check test (2)" in {
      Policy.checkValidity(NonValidPolicy2.innerPolicy()) must throwA[NonValidPolicyException].like {case e => e.getMessage === NonValidPolicy2.innerPolicy().name + " is not valid because has empty ports"}
    }
  }
  "The demo data collection policy defined with DEPOSIT" should {
    "be named 'DemoPolicy'" in {
      demo_policy.name must_== "DemoPolicy"
    }
    "have 9 concepts" in {
      demo_policy.concepts must have size 9
    }
    "have 8 data flows" in {
      demo_policy.flows must have size 8
    }

    "be valid" in {
      Policy.checkValidity(demo_policy) must not(throwA[NonValidPolicyException])
    }

    val topology = TopologyModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml")
    val predeploy_demo_policy = PreDeploy(demo_policy, topology)

    "The pre-deployed demo data collection policy issued from PreDeploy algorithm" should {
      "have 12 concepts" in {
        predeploy_demo_policy.concepts must have size 11
      }
      "have 10 data flows" in {
        predeploy_demo_policy.flows must have size 10
      }
      "have possible targets defined for all concepts" in {
        predeploy_demo_policy.concepts.map(_.readProperty("targets")) must beSome[Any].forall
      }

      "have the right repartition on network topology" in {
        predeploy_demo_policy.inputs.find(_.name equals "AC_443").get.readProperty("targets").get.asInstanceOf[Set[Entity]].map{_.name} must contain(exactly("ARD_2_443"))
        predeploy_demo_policy.concepts.collect {case x:Produce[_,_] => x}.head.readProperty("targets").get.asInstanceOf[Set[Entity]].map{_.name} must contain(exactly("RP_443_XBEE", "SmartCampus"))
      }
    }

    val deploy_demo_policy = Deploy(predeploy_demo_policy, topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
    "The deployed demo data collection policy issued from Deploy algorithm with 'Closest to the sensors' strategy" should {
      "give 3 sub-policies" in {
        deploy_demo_policy must have size 3
      }
      "give a sub-policy for ARD_1_442, ARD_2_443 and RP_443_XBEE" in {
        deploy_demo_policy.map{_.name} must contain(exactly("DemoPolicy_ARD_1_443", "DemoPolicy_ARD_2_443", "DemoPolicy_RP_443_XBEE"))
      }
      "attribute a network property for all joinpoints" in {
        deploy_demo_policy.flatMap(p => p.concepts.collect {case x:JoinPoint[_] => x}).map(_.readProperty("network")) must beSome[Any].forall
      }
      "all output join points must be associated to an input join point" in {
        val outputs_networks = deploy_demo_policy.flatMap(_.outputJoinPoints).map(_.readProperty("network").get.asInstanceOf[String]).toSeq
        val input_networks = deploy_demo_policy.flatMap(_.inputJoinPoints).map(_.readProperty("network").get.asInstanceOf[String]).toSeq

        outputs_networks must containTheSameElementsAs(input_networks)
      }

      "The operator 'Decompose'" should {
        val infrastructureModel = InfrastructureModel(topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
        val policies = Decompose(ComprehensivePolicy.innerPolicy(), infrastructureModel)

        "give 3 sub-policies" in {
          policies must have size 3
        }

        "give a sub-policy for ARD_1_442, ARD_2_443 and RP_443_XBEE" in {
          policies.map{_.name} must contain(exactly("DemoPolicy_ARD_1_443", "DemoPolicy_ARD_2_443", "DemoPolicy_RP_443_XBEE"))
        }
      }
    }


    "The deployed demo data collection policy issued from Deploy algorithm with a manual placement strategy" should {
      val demo_policy_without_dsl = ComprehensivePolicyWithoutDSL.p
      val predeployed_demo_policy_without_dsl = PreDeploy(demo_policy_without_dsl, topology)

      val policies = Deploy(predeployed_demo_policy_without_dsl, topology, Map[Concept,String](
        ComprehensivePolicyWithoutDSL.ac443         -> "ARD_2_443",
        ComprehensivePolicyWithoutDSL.door443       -> "ARD_1_443",
        ComprehensivePolicyWithoutDSL.window443     -> "ARD_2_443",
        ComprehensivePolicyWithoutDSL.collector     -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.collector2    -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.collector3    -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.temp_filter   -> "ARD_2_443",
        ComprehensivePolicyWithoutDSL.door_filter   -> "ARD_1_443",
        ComprehensivePolicyWithoutDSL.window_filter -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.produce1      -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.produce2      -> "RP_443_XBEE",
        ComprehensivePolicyWithoutDSL.produce3      -> "RP_443_XBEE"))

      "give a sub-policiy for ARD_1_442, ARD_2_443 and RP_443_XBEE" in {
        policies.map{_.name} must contain(exactly(demo_policy_without_dsl.name + "_" + "ARD_1_443", demo_policy_without_dsl.name + "_" + "ARD_2_443", demo_policy_without_dsl.name + "_" + "RP_443_XBEE"))
      }

      "associate concepts to the targeted platforms" in {
        policies.find {_.name equals demo_policy_without_dsl.name + "_" + "RP_443_XBEE"}.get.concepts.toSeq must containAllOf(Seq(
          ComprehensivePolicyWithoutDSL.collector,
          ComprehensivePolicyWithoutDSL.collector2,
          ComprehensivePolicyWithoutDSL.collector3,
          ComprehensivePolicyWithoutDSL.window_filter,
          ComprehensivePolicyWithoutDSL.produce1,
          ComprehensivePolicyWithoutDSL.produce2,
          ComprehensivePolicyWithoutDSL.produce3
        ))

      }

    }

    "The sub-policy associated to ARD_1_443" in {
      val subpolicy = deploy_demo_policy.find(_.name equals "DemoPolicy_ARD_1_443").get
      "must be named 'DemoPolicy_ARD_1_443'" in {
        subpolicy.name === "DemoPolicy_ARD_1_443"
      }

      "must have 3 concepts" in {
        subpolicy.concepts must have size 3
      }

      "must have a single output join point" in {
        subpolicy.outputJoinPoints must have size 1
      }

      "must not have input join point" in {
        subpolicy.inputJoinPoints must be empty
      }

      "has a single input named DOOR_443 that is an event sensor" in {
        subpolicy.inputs must have size 1
        subpolicy.inputs.head.isInstanceOf[EventSensor[_]] must beTrue
        subpolicy.inputs.head.asInstanceOf[EventSensor[_]].name === "DOOR_443"
      }

      "The generation of this sub-policy" in {
        val sensor = subpolicy.inputs.head.asInstanceOf[EventSensor[_]]
        "must associate DOOR_443 to a magnetic sensor" in {
          sensor.readProperty("type").get.asInstanceOf[SensorType] === SensorType.Magnetic
        }
        "must associate DOOR_443 to a Grove sensor" in {
          sensor.readProperty("brand").get.asInstanceOf[SensorBrand] === SensorBrand.GroveMagnetic
        }

        "must associate DOOR_443 to pin #1 (with mapping file)" in {
          fr.unice.modalis.cosmic.deployment.generator.Utils.lookupSensorAssignment(sensor.name) === "1"
        }
      }
    }

    "The sub-policy associated to RP_443_XBEE" in {
      val subpolicy = deploy_demo_policy.find(_.name equals "DemoPolicy_RP_443_XBEE").get

      "must have 3 input join points" in {
        subpolicy.inputJoinPoints must have size 3
      }

      "must not have output join points" in {
        subpolicy.outputJoinPoints must be empty
      }

    }
  }
}
