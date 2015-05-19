package fr.unice.modalis.cosmic.deployment.network.dsl

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */

trait TestInventory extends Inventory {
  deployed {
    aSensor withId "TEMP_442"
    aSensor withId "LIGHT_442"
    aSensorPlatform withId "ARD_1_442"
    aBridge withId "BR_442"
    aRemoteCollector withId "SmartCampusCollector"
  }
}

trait TestInfrastructure extends NetworkTopology with TestInventory{

  declare {
    "TEMP_442" isConnectedTo "ARD_1_442"
    "LIGHT_442" isConnectedTo "ARD_1_442"
    "ARD_1_442" isConnectedTo "BR_442"
    "BR_442" isConnectedTo "SmartCampusCollector"

  }
}

class NetworkTopologyTest extends SpecificationWithJUnit with TestInfrastructure{
  "An inventory stores all elements present in a network" in {

    this.resources must contain (Sensor("TEMP_442"))
    this.resources must contain (Sensor("LIGHT_442"))
    this.resources must contain (Bridge("BR_442"))
    this.resources must not contain (Sensor("INVALID"))

  }

  "A topology represents the sensor network layout" in {
    this.edges must contain (Edge(Sensor("TEMP_442"), SensorPlatform("ARD_1_442")))
    this.edges must not contain (Edge(Sensor("TEMP_442"), SensorPlatform("INVALID")))
  }

}


