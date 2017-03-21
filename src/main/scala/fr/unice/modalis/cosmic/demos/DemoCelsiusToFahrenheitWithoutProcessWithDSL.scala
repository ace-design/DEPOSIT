package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT


/**
  * This demonstration illustrates how a process can be used to convert
  * a celsius temperature into a fahrenheit temperature
  *
  * Concepts demonstrated :
  *           Process reuse
  *
  * Created by Cyril Cecchinel - I3S Laboratory on 18/11/2015.
  */
object DemoCelsiusToFahrenheitWithoutProcessWithDSL  extends DEPOSIT{

  this hasForName "CelisusToFahrenheit"
  this handles classOf[IntegerSensorType]

  val sensor = declare aPeriodicSensor() named "Sensor" withPeriod 60
  val collector = declare aCollector() named "Collector"

  val multiply = define aMultiplyBy IntegerType(9)
  val divide = define aDividerBy IntegerType(5)
  val add = define anIncrementBy IntegerType(32)

  flows {
    sensor() -> multiply("input")
    multiply("output") -> divide("input")
    divide("output") -> add("input")
    add("output") -> collector()
  }

  exportToGraphviz()
}


object ProcessTest extends App with DEPOSIT{
  val process = define aProcess(DemoCelsiusToFahrenheitWithoutProcessWithDSL())
  exportToGraphviz()


}