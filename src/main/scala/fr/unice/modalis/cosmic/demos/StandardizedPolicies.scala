package fr.unice.modalis.cosmic.demos

import fr.unice.modalis.cosmic.deposit.core._
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

/**
  * This object regroups standardized policies
  * Created by Cyril Cecchinel - I3S Laboratory on 18/11/2015.
  */
object StandardizedPolicies {

  /*
   * This policy aims at convert a raw sound sensor to a sound-level sensor
   * Input : sound / Outputs: {low,medium,high}
   */
  object RawValueToSoundSensor extends DEPOSIT {
    this hasForName "RawValueToSoundSensor"
    this handles classOf[SmartCampusType]

    val input = declare aGenericInput() named "sound"

    val filter1 = define aFilter "value < 300"
    val filter2 = define aFilter "value < 600"

    val low = declare aGenericOutput() named "low"
    val medium = declare aGenericOutput() named "medium"
    val high = declare aGenericOutput() named "high"

    flows{
      input() -> filter1("input")
      filter1("then") -> low()
      filter1("else") -> filter2("input")
      filter2("then") -> medium()
      filter2("else") -> high()
    }
  }

   /*
    * This policy aims at convert a raw sensor (ie. magnetic switch) to an opening detection sensor.
    * Input: input / Output: {open,closed}
    */
  object RawValueToOpeningSensor extends DEPOSIT {
     this hasForName "RawValueToOpeningSensor"
     this handles classOf[SmartCampusType]


     val input = declare aGenericInput() named "input"
     val filtering = define aFilter "value < 500"
     val open = declare aGenericOutput() named "open"
     val closed = declare aGenericOutput() named "closed"

     flows {
       input() -> filtering("input")
       filtering("then") -> open()
       filtering("else") -> closed()
     }

   }

   /*
    * This policy aims at convert a celsius value to a fahrenheit value
    * Input: celsius / Output: fahrenheit
    */
  object CelsiusToFahrenheit extends DEPOSIT {
     this hasForName "CelsiusToFahrenheit"
     this handles classOf[SmartCampusType]

     val input = declare aGenericInput() named "celsius"
     val multiply = define aMultiplyBy IntegerType(9)
     val divide = define aDividerBy IntegerType(5)
     val add = define anIncrementBy IntegerType(32)
     val output = declare aGenericOutput() named "fahrenheit"

     flows {
       input() -> multiply("input")
       multiply("output") -> divide("input")
       divide("output") -> add("input")
       add("output") -> output()
     }
   }

    /*
    * This policy aims at convert a celsius value to a fahrenheit value
    * Input: fahrenheit / Output: celsius
    */

  object FahrenheitToCelsius extends DEPOSIT {
    this hasForName "FahrenheitToCelsius"
    this handles classOf[SmartCampusType]

    val input = declare aGenericInput() named "fahrenheit"
    val sub = define anIncrementBy IntegerType(-32)
    val multiply = define aMultiplyBy IntegerType(5)
    val divide = define aDividerBy IntegerType(9)
    val output = declare aGenericOutput() named "celsius"

    flows {
      input() -> sub("input")
      sub("output") -> multiply("input")
      multiply("output") -> divide("input")
      divide("output") -> output()
    }
  }

}
