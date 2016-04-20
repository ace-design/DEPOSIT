package fr.unice.modalis.cosmic.simulator.sophiatech


import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder

import scala.util.Random
import scala.xml.{Elem, NodeSeq, XML}

/**
  * Large scale infrastructure simulator
  * Simulates offices and parking spaces
  * Created by Cyril Cecchinel - I3S Laboratory on 12/04/2016.
  */

protected object ParkingBoardGenerator {
  def apply(placeId:String) = {
    <entity computation="Low" name={s"BOARD_$placeId"} type="Arduino">
      <sensors>
        <sensor id={s"PRK_$placeId"} pin="1">
          <features>
            <feature>Magnetic</feature>
            <feature>GroveMagnetic</feature>
          </features>
        </sensor>
      </sensors>
      <communications>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>Out</feature>
            <feature>XBEE</feature>
            <feature>Wireless</feature>
          </features>
        </communication>
      </communications>
      <powers>
        <power>
          <features>
            <feature>Mains</feature>
          </features>
        </power>
      </powers>
      <languages>
        <language>
          <features>
            <feature>Processing</feature>
          </features>
        </language>
      </languages>
    </entity>
  }
}

protected object OfficeBoardsGenerator {
  def apply(officeId:String) = {
    <entity computation="Low" name={s"ARD_1_$officeId"} type="Arduino">
      <sensors>
        <sensor id={s"DOOR_$officeId"} pin="2">
          <features>
            <feature>GroveMagnetic</feature>
            <feature>Magnetic</feature>
          </features>
        </sensor>
        <sensor id={s"AC_$officeId"} pin="1">
          <features>
            <feature>Temperature</feature>
            <feature>GroveTemperature</feature>
          </features>
        </sensor>
        <sensor id={s"WINDOW_$officeId"} pin="2">
          <features>
            <feature>GroveMagnetic</feature>
            <feature>Magnetic</feature>
          </features>
        </sensor>
      </sensors>
      <communications>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>Out</feature>
            <feature>XBEE</feature>
            <feature>Wireless</feature>
          </features>
        </communication>
      </communications>
      <powers>
        <power>
          <features>
            <feature>Mains</feature>
          </features>
        </power>
      </powers>
      <languages>
        <language>
          <features>
            <feature>Processing</feature>
          </features>
        </language>
      </languages>
    </entity>
  }
}

protected object RelayGenerator {
  def apply(generatorID:String) = {
    <entity computation="Low" name={s"RELAY_$generatorID"} type="Raspberry">
      <sensors/>
      <communications>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>Out</feature>
            <feature>XBEE</feature>
            <feature>Wireless</feature>
          </features>
        </communication>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>In</feature>
            <feature>XBEE</feature>
            <feature>Wireless</feature>
          </features>
        </communication>
      </communications>
      <powers>
        <power>
          <features>
            <feature>Mains</feature>
          </features>
        </power>
      </powers>
      <languages>
        <language>
          <features>
            <feature>Python</feature>
          </features>
        </language>
      </languages>
    </entity>
  }
}

protected object GatewayGenerator {
  def apply(gatewayId:String) = {
    <entity computation="Low" name={s"GATEWAY_$gatewayId"} type="Raspberry">
      <sensors/>
      <communications>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>Out</feature>
            <feature>WAN</feature>
          </features>
        </communication>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>In</feature>
            <feature>XBEE</feature>
            <feature>Wireless</feature>
          </features>
        </communication>
      </communications>
      <powers>
        <power>
          <features>
            <feature>Mains</feature>
          </features>
        </power>
      </powers>
      <languages>
        <language>
          <features>
            <feature>Python</feature>
          </features>
        </language>
      </languages>
    </entity>
  }
}

protected object ServerGenerator {
  def apply(serverId:String) = {
    <entity computation="Cloud" name={s"SERVER_$serverId"} type="Server" remote="http://127.0.0.1:8000/collect">
      <sensors/>
      <communications>
        <communication>
          <features>
            <feature>Media</feature>
            <feature>Way</feature>
            <feature>In</feature>
            <feature>WAN</feature>
          </features>
        </communication>
      </communications>
      <powers>
        <power>
          <features>
            <feature>Mains</feature>
          </features>
        </power>
      </powers>
      <languages>
        <language>
          <features>
            <feature>Python</feature>
          </features>
        </language>
      </languages>
    </entity>
  }
}

protected object ConnectionGenerator {
  def apply(from:String, to:String) = <connection from={s"$from"} to={s"$to"}/>
}

object GlobalTopologyGenerator {

  def computePath(l:List[NodeSeq]):List[Elem] = l match {
    case a :: b :: tail => ConnectionGenerator(a.toString(), b.toString()) :: computePath(b :: tail)
    case a :: Nil => Nil
    case Nil => Nil
  }


  def apply(name:String, nbOffices:Int, nbParkingSpaces:Int, maxStageLevel:Int = 3, maxRelayPerStage:Int = 5, maxGateway:Int = 2, maxServer:Int = 2) = {

    def random(l:List[NodeSeq]) = Random.shuffle(l).head

    val offices = (for (i <- 1 to nbOffices) yield OfficeBoardsGenerator(i.toString)).toList
    val parkingSpaces = (for (i <- 1 to nbParkingSpaces) yield ParkingBoardGenerator(i.toString)).toList

    val servers = (for (i <- 1 to maxServer) yield ServerGenerator(i.toString)).toList
    val serversName = servers.map{e => e.map{_.attribute("name").get.head}}
    val relays = (for (i <- 1 to maxStageLevel) yield (for (j <- 1 to maxRelayPerStage) yield  RelayGenerator(s"RELAY_S${i}L$j")).toList).toList
    val relaysName = relays.map{e => e.map{_.attribute("name").get.head}}
    val gateways = (for (i <- 1 to maxGateway) yield GatewayGenerator(i.toString)).toList
    val gatewaysName = gateways.map{e => e.map{_.attribute("name").get.head}}

    // Pour toutes les plateformes de capteur
    val pathThroughRelays = (for (sensorPlatform <- offices.map(_.attribute("name").get.head) ++ parkingSpaces.map(_.attribute("name").get.head)) yield {
      // Pour tous les serveurs
      for (server <- servers.map(_.attribute("name").get.head)) yield {
        // Calculer un chemin aléatoire Platefome -> relays -> gateways (-> servers)
        List(sensorPlatform) ::: relaysName.map(random) ::: List(random(gatewaysName))
      }
    }).flatten

    val output = GatewayGenerator("OUTPUT")
    val gatewaysToOutput = for (gateway <- gatewaysName) yield ConnectionGenerator(gateway.toString(), "GATEWAY_OUTPUT")
    val outputToServers = for (server <- serversName) yield ConnectionGenerator("GATEWAY_OUTPUT", server.toString())


    val sensorsPlatformsToGateway = pathThroughRelays.flatMap(computePath)

    val allConnections = sensorsPlatformsToGateway ++ gatewaysToOutput ++ gatewaysToOutput ++ outputToServers



      <sensornetwork id="DemoTopology">
        <entities>
          {offices ++ parkingSpaces ++ relays.flatten ++ gateways ++ servers ++ output}
        </entities>
        <connections>
          {allConnections}
        </connections>
      </sensornetwork>

  }

}

object GenerateSophiaTechTopology extends App {

  val generated = GlobalTopologyGenerator("SophiaTech",
    nbOffices = 500,
    nbParkingSpaces = 400,
    maxStageLevel = 2,
    maxRelayPerStage = 3,
    maxGateway = 2,
    maxServer = 2)

  val tbegin = System.currentTimeMillis()
  val topology = TopologyModelBuilder.loadFromSpineFM(generated)
  val tend = System.currentTimeMillis()

  println(s"Elapsed: ${tend - tbegin}ms")
  XML.save("assets/configurations/SophiaTech.xml", generated)
}
