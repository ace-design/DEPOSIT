package fr.unice.modalis.cosmic.deposit.core

/**
 * Utils methods
 * Created by Cyril Cecchinel - I3S Laboratory on 05/05/15.
 */
object Utils {

  def createEventSensors[T<:DataType](prefix:String, nb:Int = 1) = for (i <- 1 to nb) yield new EventSensor[T](prefix + i)
  def createPeriodicSensors[T<:DataType](period:Int, prefix:String, nb:Int = 1) = for (i <- 1 to nb) yield new PeriodicSensor[T](period, prefix + i)
  def createCollectors[T<:DataType](prefix:String, nb:Int = 1) = for (i <- 1 to nb) yield new Collector[T](prefix + i)

}
