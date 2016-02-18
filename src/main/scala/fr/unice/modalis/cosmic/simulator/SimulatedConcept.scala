package fr.unice.modalis.cosmic.simulator

import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 18/02/2016.
  */
trait SimulatedConcept {
  val simulatedName:String
}

class SimulatedOperation[I<:DataType, O<:DataType](val simulatedName:String, val inputsNames: Set[String], val outputsNames: Set[String], val iType:Class[I], val oType:Class[O]) extends Operation[I,O] with SimulatedConcept {
  override def duplicate: Concept = new SimulatedOperation[I,O](simulatedName, inputsNames, outputsNames, iType, oType)
  override val commonName: String = "SimulatedOperation[" + inputsNames.size + "][" + outputsNames.size + "]"
}


class SimulatedSensor[I<:DataType](override val url:String, override val dataType: Class[I]) extends Sensor[I] with SimulatedConcept  {
  override val simulatedName: String = url
  override def duplicate: SimulatedSensor[I] = new SimulatedSensor[I](url, dataType)
  override val commonName: String = "SimulatedSensor[" + simulatedName + "]"
  override def ~=(x: Any): Boolean = x.isInstanceOf[SimulatedSensor[I]] && (x.asInstanceOf[SimulatedSensor[I]].url equals this.url) && (x.asInstanceOf[SimulatedSensor[I]].dataType equals this.dataType)
}

class SimulatedCollector[I<:DataType](override val name:String, override val dataType: Class[I]) extends Collector[I](name, dataType) with SimulatedConcept {
  override val simulatedName: String = "SimulatedCollector[" + simulatedName + "]"
}

object SimulatedConceptFactory {

  def apply(nbInputs:Int,nbOutput:Int, dataType:Class[_<:DataType]):Concept = apply("sim" + scala.util.Random.alphanumeric.take(5).mkString, nbInputs, nbOutput, dataType)

  def apply(name:String,nbInputs:Int,nbOutput:Int, dataType:Class[_<:DataType]):Concept = (nbInputs, nbOutput) match {
    case (a, 0) if a > 0 => new SimulatedCollector(name, dataType)
    case (0, a) if a > 0 => new SimulatedSensor(name, dataType)
    case (a, b) => new SimulatedOperation(
      name,
      (for (i <- 1 to nbInputs) yield "i" + i).toSet,
      (for (i <- 1 to nbOutput) yield "o" + i).toSet,
      dataType, dataType)
  }
}
