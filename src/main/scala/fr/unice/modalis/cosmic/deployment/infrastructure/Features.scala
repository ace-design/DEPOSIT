package fr.unice.modalis.cosmic.deployment.infrastructure

import fr.unice.modalis.cosmic.deployment.generator.{ProcessingGenerator, PythonGenerator}

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 08/01/2016.
  */
object Features {
  val featureTypeAssociation = Map(
    "Temperature" -> SensorType.Temperature,
    "Light" -> SensorType.Light,
    "Humidity" -> SensorType.Humidity,
    "Presence" -> SensorType.Presence,
    "Magnetic" -> SensorType.Magnetic
  )

  val featureBrandAssociation = Map(
    "GroveLight" -> SensorBrand.GroveLight,
    "DFLight" -> SensorBrand.DFLight,
    "DFTemperature" -> SensorBrand.DFTemperature,
    "GroveTemperature" -> SensorBrand.GroveTemperature,
    "EBTemperature" -> SensorBrand.EBTemperature,
    "PhidgetTemperature" -> SensorBrand.PhidgetTemperature,
    "GroveMagnetic" -> SensorBrand.GroveMagnetic,
    "DFMagnetic" -> SensorBrand.DFMagnetic,
    "EBMagnetic" -> SensorBrand.EBMagnetic,
    "GrovePresence" -> SensorBrand.GrovePresence,
    "EBLight" -> SensorBrand.EBLight
  )

  val featurePowerAssociation = Map(
    "MAINS" -> EntityPower.Mains,
    "BATTERY" -> EntityPower.Battery
  )

  val featureCommunicationAssociation = Map(
    "XBEE" -> CommunicationType.XBEE,
    "Zwave" -> CommunicationType.ZWave,
    "WiFi" -> CommunicationType.WiFi,
    "Serial" -> CommunicationType.Serial,
    "USB" -> CommunicationType.USB,
    "WAN" -> CommunicationType.WAN
  )

  val featureCommunicationWayAssociation = Map(
    "In" -> CommunicationWay.In,
    "Out" -> CommunicationWay.Out
  )

  val featureLanguageAssociation = Map(
    "nesC" -> ProgrammingLanguage.nesC,
    "Contiki" -> ProgrammingLanguage.Contiki,
    "Python" -> ProgrammingLanguage.Python,
    "Processing" -> ProgrammingLanguage.Processing,
    "Java" -> ProgrammingLanguage.Java,
    "Groovy" -> ProgrammingLanguage.Groovy
  )

  val codeGeneratorAssociation = Map(
    ProgrammingLanguage.Processing -> ProcessingGenerator,
    ProgrammingLanguage.Python -> PythonGenerator
  )

  object SensorType extends Enumeration {
    type SensorType = Value
    val Temperature, Light, Humidity, Presence, Magnetic, UNKNOWN = Value
  }

  object SensorBrand extends Enumeration {
    type SensorBrand = Value
    val GroveLight, DFLight, DFTemperature, GroveTemperature, EBLight, EBTemperature, PhidgetTemperature, GroveMagnetic, DFMagnetic, EBMagnetic, GrovePresence, UNKNOWN = Value
  }

  object ProgrammingLanguage extends Enumeration {
    type ProgrammingLanguage = Value
    val nesC,Contiki,Python,Processing,Java,Groovy,None = Value
  }
  object CommunicationType extends Enumeration {
    type CommunicationType = Value
    val XBEE, ZWave, WiFi, Serial, USB, WAN = Value
  }


  object CommunicationWay extends Enumeration {
    type CommunicationWay = Value
    val In, Out = Value
  }

  object EntityType extends Enumeration {
    type EntityType = Value
    val Arduino, Raspberry, CubieBoard, Server, Misc = Value
  }

  object EntityComputation extends Enumeration {
    type EntityComputation = Value
    val High, Low, Cloud = Value
  }


  object EntityPower extends Enumeration {
    type EntityPower = Value
    val Mains, Battery = Value
  }

}
