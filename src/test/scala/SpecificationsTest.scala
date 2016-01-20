import fr.unice.modalis.cosmic.demos.StandardizedPolicies
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorBrand.SensorBrand
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorType.SensorType
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deployment.network.Entity
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import org.specs2.mutable.SpecificationWithJUnit

/**
  * This is a specification test of a data collection policy
  * defined with the DEPOSIT DSL
  * Created by Cyril Cecchinel - I3S Laboratory on 20/01/2016.
  */

object ComprehensivePolicy extends DEPOSIT {

  this hasForName "DemoPolicy"
  this handles classOf[SmartCampusType]


  val ac_443 = declare aPeriodicSensor() named "AC_443" withPeriod 300
  val window_sensor = declare anEventSensor() named "WINDOW_443"
  val door_sensor = declare anEventSensor() named "DOOR_443"

  val celsiusToFahrenheit = define aProcess StandardizedPolicies.CelsiusToFahrenheit()
  val openingConverterForWindow = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
  val openingConverterForDoor = define aProcess StandardizedPolicies.RawValueToOpeningSensor()

  val threshold = define aFilter "v < 64"
  val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3")

  val collector = declare aCollector() named "Collector"

  flows {
    ac_443() -> celsiusToFahrenheit("celsius")
    celsiusToFahrenheit("fahrenheit") -> threshold("input")
    threshold("then") -> produce("i3")

    window_sensor() -> openingConverterForWindow("input")
    openingConverterForWindow("open") -> produce("i2")

    door_sensor() -> openingConverterForDoor("input")
    openingConverterForDoor("open") -> produce("i1")

    produce("output") -> collector()

  }

  // Allow the retrieval of the inner data collection policy
  def innerPolicy() = policy

}
class SpecificationsTest extends SpecificationWithJUnit{
  val demo_policy = ComprehensivePolicy.innerPolicy()

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
        predeploy_demo_policy.inputs.find(_.name equals "AC_443").get.readProperty("targets").get.asInstanceOf[Set[Entity]].map{_.name} must contain(exactly("ARD_2_443", "RP_443_XBEE", "SmartCampus"))
        predeploy_demo_policy.concepts.collect {case x:Produce[_,_] => x}.head.readProperty("targets").get.asInstanceOf[Set[Entity]].map{_.name} must contain(exactly("RP_443_XBEE", "SmartCampus"))
      }
    }

    val deploy_demo_policy = Deploy(predeploy_demo_policy, topology, DeploymentRepartition.CLOSEST_TO_SENSORS)
    "The deployed demo data collection policy issued from Deploy algorithm with 'Closest to the sensors' strategy" should {
      "give 3 sub-policies" in {
        deploy_demo_policy must have size 3
      }
      "give a sub-policies for ARD_1_442, ARD_2_443 and RP_443_XBEE" in {
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
