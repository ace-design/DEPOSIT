package fr.unice.modalis.cosmic.simulator.smartparking

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 25/03/2016.
  */
object TopologyParkingHelper {
  def generateParking(parkingNumber:String) = {
    <entity computation="Low" name={s"BOARD_$parkingNumber"} type="Arduino">
      <sensors>
        <sensor id={s"PRK_$parkingNumber"} pin="1">
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
  def generateBridge(bridgeID:String) = {
    <entity computation="High" name={s"BR_$bridgeID"} type="Raspberry">
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
  def apply(name:String, parkingRange:Range, bridgeRange:Range) = {
    <sensornetwork id={s"$name"}>
      <entities>
        {for (parking <- parkingRange) yield generateParking(parking.toString)}
        {for (bridge <- bridgeRange) yield generateBridge(bridge.toString)}
        <entity computation="Cloud" name="MonitoringServer" type="Server" remote="http://127.0.0.1:8000/collect">
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
        {for (parking <- parkingRange) yield <connection from={s"BOARD_$parking"} to={s"BR_${parking % bridgeRange.max}"}/>}
        {for (bridge <- bridgeRange) yield <connection from={s"BR_$bridge"} to="MonitoringServer"/>}
      </connections>
    </sensornetwork>
  }
}