# Automated Deployement of Data Collection Policies over Heterogeneous Shared Sensing Infrastructures 

## Article abstract
This companion page is attached to the article *Automated Deployement of Data Collection Policies over Heterogeneous Shared Sensing Infrastructures* written by Cyril Cecchinel, Philippe Collet and Sebastien Mosser, submitted to APSEC 2016.  

  Smart buildings and smart cities rely on interconnected sensor
  networks that collect data about their environment to support various
  applications.

Developing and deploying the data collection architectures of these
  systems is a challenging problem. The specificities of the sensor platforms 
  compel software engineers to work at a low level. This makes this activity tedious, producing code that
  badly exploit the network architecture, and hampering reuse of data
  collection policies.  Moreover, several data collection programs
  cannot be guaranteed to be deployable on a shared infrastructure.


In this paper, we present an automated approach that supports
  *(i)* the definition of data collection policies at a higher
  level of abstraction, *(ii)* the representation of the diverse
  platforms and the network topology, and *(iii)* the automatic
  composition and deployment of the policies on top of heterogeneous
  sensing infrastructures following different strategies. The approach is
  tooled and has been assessed on both realistic and simulated deployments.
## Introduction
The goal of this companion page is to illustrate the toolchain with an implementation of example models:
* the data collection policy (defined in Sec.2)
* the variability models depicting the sensing infrastructure (Fig 2.)
* the network topology (Fig. 1)

The source codes used during the validation are presented in annex.
## Data collection policy

### Scenario
We consider a software engineer who is in charge of programming an existing sensing infrastructure to implement several policies gathered from domain experts.  The example
focuses on a first policy that aims at monitoring energy losses in an
office, *i.e.* receiving an alert each time both the door and the window
of an office are opened while the air conditioning is powered on. This scenario will be deployed on a sensing infrastructure that is
currently executed on an academic campus where offices are equipped
with door and window opening sensors, and air
conditioning sensors.

### Implementation
The following code presents an internal DSL Scala implementation (using the [DEPOSIT DSL](https://github.com/ace-design/DEPOSIT/blob/master/src/main/scala/fr/unice/modalis/cosmic/deposit/dsl/DEPOSIT.scala)) of the data collection implementing the scenario depicted in Sec. 2.

On the first two lines, the software engineer defines the name of the data collection policy and the data-type she wants to use. Then, she declares the sensors and the collector she needs to use. The software engineer defines next the operations she wants to perform on data. Finally, she wire the data-flows between the policy elements.


```scala
this hasForName "EnergyLossAlert"
this handles classOf[SmartCampusType]

val ac = declare aPeriodicSensor() named "AC_443" withPeriod 120
val door = declare anEventSensor() named "DOOR_443"
val window = declare anEventSensor() named "WINDOW_443"
val collector = declare aCollector() named "ConnectedFaculty"

val temp_filter = define aFilter "value < 18"
val door_state= define aProcess StandardizedPolicies.RawValueToOpeningSensor()
val window_state = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
val produce = define aProducer new SmartCampusType("ALERT_AC", 1) withInputs("i1", "i2", "i3")

flows { 
  ac() -> temp_filter("input")
  door() -> door_filter("input")
  window() -> window_filter("input")
  temp_filter("then") -> produce("i1")
  window_state("open") -> produce("i2")
  door_state("open") -> produce("i3")
  produce("output") -> collector()
}
```

## Infrastructure variability

Each platform deployed in a sensing infrastructure rely on five main components: *Controller* (ie. the programming language), *Memory* (ie. the computational power), *Sensors*, *Communication* and *Power Supply*. The variability of each component is abstracted by a variability model. The variability models are expressed using the [Familiar language](http://familiar-project.github.io/) (a graphical representation of these models is depicted in the paper - Fig. 2): 


Memory:
```
SensorNetwork_Computational = FM(Computational: (Low|High|Cloud);)
```

Sensors:
```
SensorNetwork_Sensor = FM(Sensor: (Light|Temperature|Magnetic|Presence); Light: (GroveLight|DFLight); Temperature: (GroveTemperature | DFTemperature | EBTemperature | PhidgetTemperature); Magnetic: (GroveMagnetic | DFMagnetic | EBMagnetic); Presence: GrovePresence;)
```

Controller:
```
SensorNetwork_Programming = FM(Programming: (nesC|Contiki|Python|Processing|Java|Groovy);)
```

Communication:
```
SensorNetwork_Communication = FM(Communication: Media Way; Media: (Wireless|Wired|WAN); Wireless: (XBEE|ZWave|WiFi); Wired: (Serial|USB);  Way: (In|Out);)
```

Power supply:
```
SensorNetwork_Power = FM(Power: (Battery|Mains);)
```

We have also represented the platform type as a feature model:
```
SensorNetwork_Platform = FM(Platform: (Arduino|Raspberry|telosB|CubieBoard|Server|Misc);)
```


The constraint relationships between these variability models are handled by [SpineFM](http://modalis.i3s.unice.fr/spinefm). The example bellow shows the implementation of some constraint relationships.

```
source=Platform
target=Programming

SELECTED Arduino => SELECT Wiring;
SELECTED telosB => SELECT nesC;
```

(ie. The selection of Arduino triggers the selection of the Wiring programming language)

```
source=Platform
target=Computational

SELECTED Arduino => SELECT Low;
```

```
source=Platform
target=Computational

SELECTED Raspberry => SELECT Medium;
```

After the features selection, a XML file describing the platforms facilities is generated. The example bellow presents the XML description of the *ARD_1_443* platform. The platform is based on *Arduino* and thus, according the constraint relationships, has *low memory* and needs to be programming using the *Wiring language*. This platform has been configured to send data wirelessly using the *Zigbee protocol* and has a *mains* power source. Two sensors are integrated in *ARD_1_443*: an *Grove Presence sensor* (PRESENCE_443) and a *Grove magnetic sensor* (DOOR_443)

After the selection, an XML file describing the platforms facilities is generated. The example bellow shows the generated XML for the *ARD_1_443* platform:

```xml
        <entity computation="Low" name="ARD_1_443" type="Arduino">
            <sensors>
                <sensor id="DOOR_443" pin="2">
                    <features>
                        <feature>GroveMagnetic</feature>
                        <feature>Magnetic</feature>
                    </features>
                </sensor>
                <sensor id="PRESENCE_443" pin="1">
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
```

## Network topology

The network topology is assimilated to a graph where vertices represent platforms and edges represent a connectibity between two platforms. The following XML file presents the implementation of the newtork topology depicted in Fig. 1:

```xml
<sensornetwork id="ConnectedFacultyNetwork">
   <connections>
      <connection from="ARD_1_443" to="BRIDGE_1"/>
      <connection from="ARD_2_443" to="BRIDGE_1"/>
      <connection from="ARD_1_442" to="BRIDGE_1"/>
      <connection from="BRIDGE_1" to="ConnectedFaculty"/>
      <connection from="ARD_1_444" to="ConnectedFaculty"/>
   </connections>
</sensornetwork>
```

## Annexes
### Template source code
The following code is a Scala implementation of the template data collection policy (Sec 4.A.).
```scala
object TemplatePolicy{
	def apply(officeNumber:Int) = {
		this hasForName s"EnergyLossAlert_$officeNumber"
		this handles classOf[SmartCampusType]

		val ac = declare aPeriodicSensor() named s"AC_$officeNumber" withPeriod 120
		val door = declare anEventSensor() named s"DOOR_$officeNumber"
		val window = declare anEventSensor() named s"WINDOW_$officeNumber"
		val collector = declare aCollector() named "ConnectedFaculty"

		val temp_filter = define aFilter "value < 18"
		val door_state= define aProcess StandardizedPolicies.RawValueToOpeningSensor()
		val window_state = define aProcess StandardizedPolicies.RawValueToOpeningSensor()
		val produce = define aProducer new SmartCampusType(s"ALERT_AC_$officeNumber", 1) 	withInputs("i1", 	"i2", "i3")

		flows { 
		  ac() -> temp_filter("input")
		  door() -> door_filter("input")
		  window() -> window_filter("input")
		  temp_filter("then") -> produce("i1")
		  window_state("open") -> produce("i2")
		  door_state("open") -> produce("i3")
		  produce("output") -> collector()
		}
	}
}
```

This template is instanciated for a given office (*e.g.* office 443) as follows:
```scala
val office443_policy = TemplatePolicy(443)
```
### Generating the comprehensive policy (without composition)
The comprehensive policy (without composition) can be accessed [here](https://github.com/ace-design/DEPOSIT/blob/master/src/main/scala/fr/unice/modalis/cosmic/simulator/smartbuilding/ComprehensivePolicy.scala)

### Generating the comprehensive policy (with composition)
The comprehensive policy can be inlined as follow:

```scala
val policy = ExperimentalValues.RANGE_OFFICE.foldLeft(new Policy("")){ (acc, e) => acc ++ TemplatePolicy(e)}
```
where RANGE_OFFICE = 1 to 50.

In this case, we compose 50 times the TemplatePolicy that has been instanciated for each office
