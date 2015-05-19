package fr.unice.modalis.cosmic.deployment.network.dsl.sample

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{Inventory, Bridge, Sensor, NetworkTopology}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */
object Demo extends App with SmartCampusInfrastructure {
  println(edges)
}

trait SmartCampusInfrastructure extends NetworkTopology with SmartCampusInventory{

   declare {
     "TEMP_442" isConnectedTo "ARD_1_442"
     "LIGHT_442" isConnectedTo "ARD_1_442"
     "TEMP_443" isConnectedTo "ARD_1_442"
     "LIGHT_CAFE" isConnectedTo "ARD_1_CAFE"
     "TEMP_CAFE" isConnectedTo "ARD_1_CAFE"
     "TEMP_444" isConnectedTo "ARD_1_444"
     "LIGHT_444" isConnectedTo "ARD_1_444"

     "ARD_1_442" isConnectedTo "BR_442"
     "ARD_1_444" isConnectedTo "BR_444"
     "ARD_1_CAFE" isConnectedTo "BR_CAFE"

     "BR_442" isConnectedTo "SmartCampusCollector"
     "BR_444" isConnectedTo "SmartCampusCollector"
     "BR_CAFE" isConnectedTo "SmartCampusCollector"


  }
}

trait SmartCampusInventory extends Inventory {
  deployed {
    aSensor withId "TEMP_442"
    aSensor withId "LIGHT_442"
    aSensor withId "TEMP_443"
    aSensor withId "LIGHT_CAFE"
    aSensor withId "TEMP_CAFE"
    aSensor withId "TEMP_444"
    aSensor withId "LIGHT_444"


    aSensorPlatform withId "ARD_1_442"
    aSensorPlatform withId "ARD_1_444"
    aSensorPlatform withId "ARD_1_CAFE"

    aBridge withId "BR_442"
    aBridge withId "BR_444"
    aBridge withId "BR_CAFE"

    aRemoteCollector withId "SmartCampusCollector"
  }
}