package fr.unice.modalis.cosmic.deployment.utils

import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{EntityComputation, EntityType}
import fr.unice.modalis.cosmic.deployment.infrastructure.{Features, NetworkTopology}
import fr.unice.modalis.cosmic.deployment.network.{Communication, Edge, Entity, Sensor}

import scala.xml.{Elem, NodeSeq, XML}


/**
  * This object creates a topology model from a SpineFM feature model (XML representation)
  * Created by Cyril Cecchinel - I3S Laboratory on 28/05/15.
  */
object TopologyModelBuilder {

  /**
    * Build the topology
    * @param pathToFile Path to XML representation
    * @return a Network Topology object
    */
  def apply(pathToFile: String) = loadFromSpineFM(pathToFile)

  /**
    * Find a feature into an XML representation
    * @param name Feature to lookup
    * @param seq XML representation
    * @return Feature found ?
    */
  def findFeature(name: String, seq: NodeSeq) = (seq \\ "feature").exists(_.text.toLowerCase == name.toLowerCase)


  /**
    * Load the configuration
    * @param configuration XML representation
    * @return a Network Topology object
    */
  def loadFromSpineFM(configuration: Elem):NetworkTopology = {
    def buildSensor(name: String, pin: Option[String], s: NodeSeq) = {
      // Get type
      val sensorType = (for (k <- Features.featureTypeAssociation.keys if findFeature(k, s \\ "features")) yield Features.featureTypeAssociation(k)).head
      val sensorBrand = (for (k <- Features.featureBrandAssociation.keys if findFeature(k, s \\ "features")) yield Features.featureBrandAssociation(k)).head

      new Sensor(name, sensorType, sensorBrand, pin)
    }

    def buildEntity(id: String, sensors: Seq[Sensor], computation: EntityComputation.Value, etype: EntityType.Value, s: NodeSeq) = {

      def buildCommunications(seq: NodeSeq) = {
        for (c <- seq \\ "communication") yield {
          val communicationType = (for (k <- Features.featureCommunicationAssociation.keys if findFeature(k, c)) yield Features.featureCommunicationAssociation(k)).head
          val communicationWay = (for (k <- Features.featureCommunicationWayAssociation.keys if findFeature(k, c)) yield Features.featureCommunicationWayAssociation(k)).head
          Communication(communicationType, communicationWay)
        }
      }

      val entityPower = (for (k <- Features.featurePowerAssociation.keys if findFeature(k, s \\ "powers" \\ "power")) yield Features.featurePowerAssociation(k)).head
      val entityCommunications = buildCommunications(s \\ "communications")
      val entityLanguages = (for (k <- Features.featureLanguageAssociation.keys if findFeature(k, s \\ "languages" \\ "language")) yield Features.featureLanguageAssociation(k)).head
      val entityPublicUrl = s \@ "remote"
      new Entity(id, sensors.toSet, entityPower, entityCommunications.toSet, computation, etype, entityLanguages, if (!entityPublicUrl.isEmpty) Some(entityPublicUrl) else None)
    }


    val name = (configuration \\ "sensornetwork" \ "@id").text



    val entities = (configuration \\ "sensornetwork" \ "entities" \ "entity").map(n =>
      buildEntity((n \\ "@name").text,
        for (s <- n \\ "sensors" \\ "sensor"; sid = (s \ "@id").text; pin = s \@ "pin"; spin = if (pin.isEmpty) None else Some(pin)) yield buildSensor(sid, spin, s),
        computation = EntityComputation.withName((n \\ "@computation").text),
        etype = EntityType.withName((n \\ "@type").text),
        n))


    val edges = (configuration \\ "sensornetwork" \ "connections" \ "connection").map(n => {
      val source = (n \\ "@from").text
      val destination = (n \\ "@to").text

      Edge(source, destination)
    }

    )


    new NetworkTopology(name, entities.toSet, edges.toSet)
  }

  /**
    * Load the configuration
    * @param pathToFile Path to XML representation
    * @return a Network Topology object
    */
  def loadFromSpineFM(pathToFile: String):NetworkTopology = loadFromSpineFM(XML.loadFile(pathToFile))
}
