package fr.unice.modalis.cosmic.deployment.utils

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
  def findFeature(name:String, seq:NodeSeq) = (seq \\ "feature").find(_.text.toLowerCase == name.toLowerCase).isDefined

  /**
   * Load the configuration
   * @param pathToFile Path to XML representation
   * @return a Network Topology object
   */
  def loadFromSpineFM(pathToFile: String) = {

    val configuration = XML.loadFile(pathToFile)

    // Let's build the topology
      new NetworkTopology{

        // First: Build the inventory
        deployed {

          def buildSensorPlatform(id: String, seq: NodeSeq) = {
            val sensorplatform = aSensorPlatform() withId id
            if (findFeature("ProgrammableSP", seq)) sensorplatform.addProperty("programmable", true)

            sensorplatform
          }

          def buildBridge(id: String, seq: NodeSeq) = {
            val bridge = aBridge() withId id
            if (findFeature("ProgrammableBR", seq)) bridge.addProperty("programmable", true)

            bridge
          }

          def buildNode(id: String, seq: NodeSeq) = {
            if (findFeature("Sensor", seq))
              aSensor() withId id

            else if (findFeature("SensorPlatform", seq))
              buildSensorPlatform(id, seq)

            else if (findFeature("Bridge", seq))
              buildBridge(id, seq)

            else if (findFeature("Remote", seq))
              aRemoteCollector() withId id


            else throw new Exception("Unable to build node " + id)

          }

          for (e <- (configuration \\ "sensornetwork" \\ "entities" \\ "entity"); id = (e \\ "@id").text) {
            buildNode(id, e \\ "features")
          }
        }

        // Secondly: Build the links
        declare {
          for (l <- (configuration \\ "sensornetwork" \\ "connections" \\ "connection"); from = (l \\ "@from").text; to = (l \\ "@to").text) {
            from isConnectedTo to
          }
        }

      }

  }
}
