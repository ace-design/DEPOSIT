package fr.unice.modalis.cosmic.simulator.smartparking

import fr.unice.modalis.cosmic.simulator.dsl.DEPOSITSimulator

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 29/02/2016.
  */
object SmartParkingDemo extends DEPOSITSimulator {

  create aSmartParkingScenario() having 150 parkingSpaces() distributedIn 20 districts()

}


