package fr.unice.modalis.cosmic.deployment.network.dsl.sample

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel.{Inventory, Bridge, Sensor, NetworkTopology}

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */
object Demo extends App with SmartCampusInventory {
  println(resources)
}

trait SmartCampusInfrastructure extends NetworkTopology with SmartCampusInventory{

  /* declare {
    "ARD_1_442" sendsDataTo "BR_442"
    "ARD_1_443" sendsDataTo "BR_442"
    "BR_442" sendsDataTo "SC1"
  } */
}

trait SmartCampusInventory extends Inventory {
  declare {
    aSensor withId "TEMP_442"
    aSensorPlatform withId "ARD_1_442"
    aBridge withId "BR_442"
    aRemoteCollector withId "SC1"
  }
}