package fr.unice.modalis.cosmic.deployment.utils

import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{EntityComputation, EntityType}
import fr.unice.modalis.cosmic.deployment.infrastructure.{Features, NetworkTopology}
import fr.unice.modalis.cosmic.deployment.network.dsl.kernel._

import scala.xml.{NodeSeq, XML}


/**
  * This object creates an infrastructure model from a SpineFM feature model (XML representation)
  * Created by Cyril Cecchinel - I3S Laboratory on 28/05/15.
  */
object InfrastructureModelBuilder {

  /**
    * Build the infrastructure
    * @param pathToFile Path to XML representation
    * @return a Network Topology object
    */
  def apply(pathToFile:String) = loadFromSpineFM(pathToFile)

  /**
    * Find a feature into an XML representation
    * @param name Feature to lookup
    * @param seq XML representation
    * @return Feature found ?
    */
  def findFeature(name:String, seq:NodeSeq) = (seq \\ "feature").exists(_.text.toLowerCase == name.toLowerCase)

  /**
    * Load the configuration
    * @param pathToFile Path to XML representation
    * @return a Network Topology object
    */
  def loadFromSpineFM(pathToFile: String) = {

    def buildSensor(name: String, s: NodeSeq) = {
      // Get type
      val sensorType = (for (k <- Features.featureTypeAssociation.keys if findFeature(k, s \\ "features")) yield Features.featureTypeAssociation(k)).head
      val sensorBrand = (for (k <- Features.featureBrandAssociation.keys if findFeature(k, s \\ "features")) yield Features.featureBrandAssociation(k)).head

      new Sensor(name, sensorType, sensorBrand)
    }

    def buildEntity(id: String, sensors: Seq[Sensor], computation: EntityComputation.Value, etype: EntityType.Value, s:NodeSeq) = {

      def buildCommunications(seq: NodeSeq) = {
        for (c <- seq \\ "communication") yield {
          val communicationType = (for (k <- Features.featureCommunicationAssociation.keys if findFeature(k, c)) yield Features.featureCommunicationAssociation(k)).head
          val communicationWay = (for (k <- Features.featureCommunicationWayAssociation.keys if findFeature(k, c)) yield Features.featureCommunicationWayAssociation(k)).head
          Communication(communicationType, communicationWay)
        }
      }

      val entityPower = (for (k <- Features.featurePowerAssociation.keys if findFeature(k, s \\ "powers" \\ "power")) yield Features.featurePowerAssociation(k)).head
      val entityCommunications = buildCommunications (s \\ "communications")
      val entityLanguages = (for (k <- Features.featureLanguageAssociation.keys if findFeature(k, s \\ "languages" \\ "language")) yield Features.featureLanguageAssociation(k)).head
      new Entity(id,sensors.toSet,entityPower, entityCommunications.toSet, computation, etype, entityLanguages)
    }



    val configuration = XML.loadFile(pathToFile)


    val entities = for (e <- configuration \\ "sensornetwork" \\ "entities" \\ "entity";
                        id = (e \\ "@name").text;
                        computation = EntityComputation.withName((e \\ "@computation").text);
                        etype = EntityType.withName((e \\ "@type").text)) yield {


      val sensors = for (s <- e \\ "sensors" \\ "sensor"; sid = (s \\ "@id").text) yield buildSensor(sid, s)

      buildEntity(id, sensors, computation, etype, e)
    }

    val edges = for (e <- configuration \\ "connections" \\ "connection";
                     source = entities find (_.name equals (e \\ "@from").text);
                     target = entities find (_.name equals (e \\ "@to").text)
                     if source.isDefined && target.isDefined) yield Edge(source.get, target.get)

    new NetworkTopology(entities.toSet, edges.toSet)




  }
}
