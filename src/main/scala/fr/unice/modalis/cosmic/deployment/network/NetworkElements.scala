package fr.unice.modalis.cosmic.deployment.network

import fr.unice.modalis.cosmic.deployment.infrastructure.Features.CommunicationType.CommunicationType
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.CommunicationWay.CommunicationWay
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.EntityComputation.EntityComputation
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.EntityPower.EntityPower
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.EntityType.EntityType
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.ProgrammingLanguage.ProgrammingLanguage
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorBrand.SensorBrand
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.SensorType.SensorType
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{EntityType, ProgrammingLanguage, SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deployment.network.Media.Media
import fr.unice.modalis.cosmic.deposit.core.Properties
/**
 * Represent sensing infrastructure nodes
 * Created by Cyril Cecchinel - I3S Laboratory on 13/05/15.
 */



trait GenericNode extends Properties{
  def isProgrammable:Boolean = readProperty("programmable").getOrElse(false).asInstanceOf[Boolean]
  val name:String
}


case class Entity(name:String,
                  sensors:Set[Sensor],
                  power:EntityPower,
                  communication:Set[Communication],
                  computation:EntityComputation,
                  eType:EntityType = EntityType.Misc,
                  language:ProgrammingLanguage = ProgrammingLanguage.None) extends GenericNode {

  override def isProgrammable:Boolean = language != ProgrammingLanguage.None
}


case class Communication(cType:CommunicationType, cWay:CommunicationWay)




case class Sensor(name:String, sType:SensorType = SensorType.UNKNOWN, sBrand:SensorBrand = SensorBrand.UNKNOWN, sPin:Option[String] = None) extends GenericNode

case class Edge(source: Entity, destination:Entity, media:Media = Media.Unknown)

object Media extends Enumeration {
  type Media = Value
  val XBee, Serial, ZWave, USB, Unknown = Value
}
