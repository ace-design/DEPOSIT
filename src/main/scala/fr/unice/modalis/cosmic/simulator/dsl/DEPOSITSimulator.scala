    package fr.unice.modalis.cosmic.simulator.dsl

    import com.typesafe.scalalogging.LazyLogging
    import fr.unice.modalis.cosmic.deposit.core._
    import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

    /**
      * Created by Cyril Cecchinel - I3S Laboratory on 29/02/2016.
      */
    trait DEPOSITSimulator extends LazyLogging{

      var globalScenario:Option[ConcreteScenario] = None

      def apply() = {
        if (globalScenario.isEmpty) throw new Exception("No scenario has been built")
        globalScenario.get.build()
        globalScenario.get()

      }

      /**
        * Create a simulation
        * @return A SimulationBuilderObject
        */
      def create() = {
        SimulationBuilder()
      }

      /**
        * Generic SimulationBuilder
        * @param simulationContext Type of simulation (default: None)
        */
      protected case class SimulationBuilder(simulationContext: SimulationContext.Value = SimulationContext.UNKNOWN) {
        def aSmartParkingScenario() = {

          this.copy(simulationContext = SimulationContext.SMART_PARKING)
          SensorBuilder(SimulationContext.SMART_PARKING)
        }
      }


      /**
        * Generic SensorBuilder
        * @param simulationContext Type of simulation
        */
      protected case class SensorBuilder(simulationContext: SimulationContext.Value) {
        def having(sensors:Int) = {
          assert(sensors > 1)
          simulationContext match {
            case SimulationContext.SMART_PARKING => {
              val scenario = SmartParkingSensorBuilder(sensors)
              globalScenario = Some(scenario)
              scenario
            }
          }
        }

        /**
          * SmartParking sensor builder (specific)
          * @param sensorQuantity Number of parking sensors
          * @param districtQuantity Number of districts
          */
        protected case class SmartParkingSensorBuilder(sensorQuantity:Int = 0, districtQuantity:Int = 1, threshold:Option[Int] = None) extends ConcreteScenario{
          def parkingSpaces() = this
          def districts() = this

          def withAThresholdValue() = SmartParkingThresholdBuilder(this)
          def distributedIn(totalDistricts:Int) = {
            val scenario = this.copy(districtQuantity = totalDistricts)
            globalScenario = Some(scenario)
            scenario
          }

          case class SmartParkingThresholdBuilder(builder: SmartParkingSensorBuilder) {
            def of(value: Int) = {
              val scenario = SmartParkingSensorBuilder(sensorQuantity, districtQuantity, Some(value))
              globalScenario = Some(scenario)
              scenario
            }
          }

          override def build(): Unit = {
            assert(districtQuantity > 0)

            hasForName("SmartParkingSimulation_" + System.currentTimeMillis / 1000)
            handles(classOf[SmartCampusType])

            (1 to sensorQuantity) map {i => declare anEventSensor() named "PRK_" + i}
            for (district <- 1 to districtQuantity) {
              logger.debug(s"Creating district #$district")
              val range = 1 to (if (district != districtQuantity) sensorQuantity / districtQuantity else sensorQuantity / districtQuantity + sensorQuantity % districtQuantity)
              val inputs = for (input <- range) yield "i" + input
              define anAdder() withInputs(inputs.map{e => e}:_*)
              flush()
              lastOperation.get.addProperty("name", "ADDER_DISTRICT_" + district)
              val sensors = (for (idx <- range) yield "PRK_" + ((district - 1) * (sensorQuantity / districtQuantity) + idx)).map {name => policy.findSensorByName(name).get}


              var input:Int = 1
              for (s <- sensors) {
                val flow = Flow(s.output.asInstanceOf[Output[SmartCampusType]],lastOperation.get.getInput("i" + input).asInstanceOf[Input[SmartCampusType]])
                input = input + 1
                policy = policy.addFlow(flow)
              }
            }
            if (districtQuantity == 1) {
              val adder = policy.concepts.collectFirst {case x:Add[_] => x}.get
              val collector = new Collector("DemoCollector", classOf[SmartCampusType])
              policy = policy.add(collector).add(Flow(adder.output, collector.input))
            }
            else {
              val aggregator = new Add((for (i <- 1 to districtQuantity) yield "i" + i).toSet,classOf[SmartCampusType])

              var inputAggregator:Int = 1
              var flows = Set[Flow[SmartCampusType]]()
              for (add  <- policy.concepts.collect {case x:Add[_] => x}) {
                flows = flows + Flow(add.output.asInstanceOf[Output[SmartCampusType]], aggregator.getInput("i" + inputAggregator))
                inputAggregator = inputAggregator + 1
              }

              policy = policy.add(aggregator)
              flows.foreach(f => policy = policy.add(f))

              val collector = new Collector("DemoCollector", classOf[SmartCampusType])
              policy = policy.add(collector).add(Flow(aggregator.output, collector.input))
            }

            if (threshold.isDefined){
              val limit = 1 + (sensorQuantity / districtQuantity) * threshold.get / 100
              logger.debug(s"Setting a threshold of $limit parking spaces for each district")
              // Lookup for each district adder
              val districtAdders = policy.operations.filter(o => o.properties.exists(p => p.name.equals("name") && p.value.asInstanceOf[String].startsWith("ADDER_DISTRICT_")))
              for (adder <- districtAdders) {
                val filter = Conditional(s"value < $limit", classOf[SmartCampusType])
                policy = policy.add(filter).add(Flow(adder.getOutput().asInstanceOf[Output[SmartCampusType]],filter.getInput()))
                val remoteScreen = new Collector("REMOTE_SCREEN", classOf[SmartCampusType])
                val flow = new Flow(filter.getOutput("then"), remoteScreen.input)
                policy = policy.add(remoteScreen).add(flow)
              }
            }
          }
        }
      }


      protected object SimulationContext extends Enumeration {
        val SMART_PARKING, UNKNOWN = Value
      }

    }

    trait ConcreteScenario extends DEPOSIT {
      def build():Unit
    }