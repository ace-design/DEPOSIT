package fr.unice.modalis.cosmic.simulator.smartbuilding

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 21/03/2016.
  */
protected object TopologyHelper {

  def apply(name:String, officeRange:Range, bridgeRange:Range) = {
    <sensornetwork id={s"$name"}>
    <entities>
      {for (office <- officeRange) yield generateOfficeBoard(office.toString)}
      {for (bridge <- bridgeRange) yield generateBridge(bridge.toString)}
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
        {for (office <- officeRange) yield <connection from={s"ARD_1_$office"} to=""/><connection from={s"ARD_2_$office"} to=""/>}
        {for (bridge <- bridgeRange) yield <connection from={s"RP_$bridge"} to=""/>}
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
