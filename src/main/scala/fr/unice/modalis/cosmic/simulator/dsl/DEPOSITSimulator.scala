package fr.unice.modalis.cosmic.simulator.dsl

import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 29/02/2016.
  */
trait DEPOSITSimulator extends DEPOSIT{

  /**
    * Create a simulation
    * @return A SimulationBuilderObject
    */
  def create() = {
    SimulationBuilder()
  }

  /**
    * Generic SimulationBuilder
    * @param simulationContext Type of simulation (default: None)
    */
  protected case class SimulationBuilder(simulationContext: SimulationContext.Value = SimulationContext.UNKNOWN) {
    def aSmartParkingScenario() = {
      this.copy(simulationContext = SimulationContext.SMART_PARKING)
      SensorBuilder(SimulationContext.SMART_PARKING)
    }
  }


  /**
    * Generic SensorBuilder
    * @param simulationContext Type of simulation
    */
  protected case class SensorBuilder(simulationContext: SimulationContext.Value) {
    def having(sensors:Int) = {
      simulationContext match {
        case SimulationContext.SMART_PARKING => SmartParkingSensorBuilder(sensors)
      }
    }

    /**
      * SmartParking sensor builder (specific)
      * @param sensorQuantity Number of parking sensors
      * @param districtQuantity Number of districts
      */
    protected case class SmartParkingSensorBuilder(sensorQuantity:Int = 0, districtQuantity:Int = 0) {
      def parkingSpaces() = this
      def districts() = this
      def distributedIn(i:Int) = this.copy(districtQuantity = i)
    }

  }


  protected object SimulationContext extends Enumeration {
    val SMART_PARKING, UNKNOWN = Value
  }

}