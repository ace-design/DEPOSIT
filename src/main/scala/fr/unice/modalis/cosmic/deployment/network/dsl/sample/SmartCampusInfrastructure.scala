package fr.unice.modalis.cosmic.deployment.network.dsl.sample

import fr.unice.modalis.cosmic.deployment.network.dsl.kernel._

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 19/05/15.
 */
//noinspection ScalaDefaultFileTemplateUsage
object Demo extends App {
  /* Actions to perform with the Infrastructure model */

}

class SmartCampusInfrastructure extends NetworkTopology{
    deployed {
      aSensor withId "TEMP_442"
      aSensor withId "LIGHT_443"
      aSensor withId "TEMP_443"
      aSensor withId "DOOR_443"
      aSensor withId "PRESENCE_443"
      aSensor withId "WINDOW_443"
      aSensor withId "LIGHT_CAFE"
      aSensor withId "TEMP_CAFE"
      aSensor withId "TEMP_444"
      aSensor withId "LIGHT_444"
      aSensor withId "AC_443"


      aSensorPlatform withId "ARD_1_442"
      aSensorPlatform withId "ARD_1_443"
      aSensorPlatform withId "ARD_2_443"
      aSensorPlatform withId "ARD_3_443"
      aSensorPlatform withId "ARD_1_444"
      aSensorPlatform withId "ARD_1_CAFE"

      aBridge withId "BR_443_1"
      aBridge withId "BR_443_2"
      aBridge withId "BR_CAFE"

      aRemoteCollector withId "SmartCampusCollector"
    }
   declare {
     "TEMP_442" isConnectedTo "ARD_1_442"

     "TEMP_443" isConnectedTo "ARD_1_443"
     "LIGHT_443" isConnectedTo "ARD_1_443"

     "DOOR_443" isConnectedTo "ARD_2_443"
     "PRESENCE_443" isConnectedTo "ARD_2_443"
     "WINDOW_443" isConnectedTo "ARD_3_443"
     "AC_443" isConnectedTo "ARD_3_443"

     "LIGHT_CAFE" isConnectedTo "ARD_1_CAFE"
     "TEMP_CAFE" isConnectedTo "ARD_1_CAFE"
     "TEMP_444" isConnectedTo "ARD_1_444"
     "LIGHT_444" isConnectedTo "ARD_1_444"

     "ARD_1_442" isConnectedTo "BR_443_1" by "Serial"
     "ARD_1_443" isConnectedTo "BR_443_1" by "Serial"

     "ARD_2_443" isConnectedTo "BR_443_2" by "Xbee"
     "ARD_3_443" isConnectedTo "BR_443_2" by "Xbee"
     "ARD_1_444" isConnectedTo "BR_443_2" by "Xbee"


     "ARD_1_CAFE" isConnectedTo "BR_CAFE"

     "BR_443_1" isConnectedTo "SmartCampusCollector"
     "BR_443_2" isConnectedTo "SmartCampusCollector"
     "BR_CAFE" isConnectedTo "SmartCampusCollector"


  }
}
