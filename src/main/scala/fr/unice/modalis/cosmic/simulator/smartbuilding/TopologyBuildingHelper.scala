package fr.unice.modalis.cosmic.simulator.smartbuilding

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */

object TopologyBuildingHelper {



  def apply(name:String, officeRange:Range, bridgeRange:Range, gatewayRange:Range) = {
    <sensornetwork id={s"$name"}>
    <entities>
      {for (office <- officeRange) yield generateOfficeBoard(office.toString)}
      {for (bridge <- bridgeRange) yield generateBridge(bridge.toString)}
      {for (gateway <- gatewayRange) yield generateGateway(gateway.toString)}

      <entity computation="Cloud" name="SmartCampus" type="Server" remote="http://127.0.0.1:8000/collect">
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
    </entities>
      <connections>
        {for (office <- officeRange) yield <connection from={s"ARD_1_$office"} to={s"RP_${office % bridgeRange.max}"}/><connection from={s"ARD_2_$office"} to={s"RP_${office % bridgeRange.max}"}/>}
        {for (bridge <- bridgeRange) yield <connection from={s"RP_$bridge"} to={s"GATEWAY_${bridge % gatewayRange.max}"}/>}
        {for (gateway <- gatewayRange) yield <connection from={s"GATEWAY_$gateway"} to={"SmartCampus"}/>}
      </connections>
    </sensornetwork>
  }

  def generateBridge(bridgeID:String) = {
    <entity computation="High" name={s"RP_$bridgeID"} type="Raspberry">
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
  def generateGateway(gatewayID:String) = {
    <entity computation="High" name={s"GATEWAY_$gatewayID"} type="Raspberry">
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
        <feature>Python</feature>
        </language>
      </languages>
    </entity>
  }
  def generateOfficeBoard(officeNumber:String) = {
    <entity computation="Low" name={s"ARD_1_$officeNumber"} type="Arduino">
      <sensors>
        <sensor id={s"DOOR_$officeNumber"} pin="2">
          <features>
            <feature>GroveMagnetic</feature>
            <feature>Magnetic</feature>
          </features>
        </sensor>
        <sensor id={s"PRESENCE_$officeNumber"} pin="1">
          <features>
            <feature>Presence</feature>
            <feature>GrovePresence</feature>
          </features>
        </sensor>
        <sensor id={s"LIGHT_$officeNumber"} pin="3">
          <features>
            <feature>Presence</feature>
            <feature>GrovePresence</feature>
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
      <entity computation="Low" name={s"ARD_2_$officeNumber"} type="Arduino">
        <sensors>
          <sensor id={s"WINDOW_$officeNumber"} pin="2">
            <features>
              <feature>GroveMagnetic</feature>
              <feature>Magnetic</feature>
            </features>
          </sensor>
          <sensor id={s"TEMP_$officeNumber"} pin="3">
            <features>
              <feature>GroveTemperature</feature>
              <feature>Temperature</feature>
            </features>
          </sensor>
          <sensor id={s"AC_$officeNumber"} pin="4">
            <features>
              <feature>GroveTemperature</feature>
              <feature>Temperature</feature>
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
