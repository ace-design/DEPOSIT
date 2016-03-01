package fr.unice.modalis.cosmic.simulator.smartparking
import fr.unice.modalis.cosmic.simulator.dsl.DEPOSITSimulator


/**
  * Created by Cyril Cecchinel - I3S Laboratory on 29/02/2016.
  */

object ParkingExample extends DEPOSITSimulator {
  create aSmartParkingScenario() having 100 parkingSpaces() distributedIn 5 districts() withAThresholdValue() of 30
}

object SmartParkingDemo extends App{

  ParkingExample().exportToGraphviz()


}


