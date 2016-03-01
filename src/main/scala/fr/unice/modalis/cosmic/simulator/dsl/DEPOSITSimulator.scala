    package fr.unice.modalis.cosmic.simulator.dsl

    import com.typesafe.scalalogging.LazyLogging
    import fr.unice.modalis.cosmic.deposit.core._
    import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT

    /**
      * Created by Cyril Cecchinel - I3S Laboratory on 29/02/2016.
      */
    trait DEPOSITSimulator extends DEPOSIT with LazyLogging{


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
          hasForName("SmartParkingSimulation_" + System.currentTimeMillis / 1000)
          handles(classOf[SmartCampusType])
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
              (1 to sensors) map {i => declare anEventSensor() named "PRK_" + i}
              SmartParkingSensorBuilder(sensors)
            }
          }
        }

        /**
          * SmartParking sensor builder (specific)
          * @param sensorQuantity Number of parking sensors
          * @param districtQuantity Number of districts
          */
        protected case class SmartParkingSensorBuilder(sensorQuantity:Int = 0, districtQuantity:Int = 1) {
          def parkingSpaces() = this
          def districts() = this

          def withAThresholdValue() = SmartParkingThresholdBuilder(this)
          def distributedIn(totalDistricts:Int) = {
            assert(totalDistricts > 0)
            for (district <- 1 to totalDistricts) {
              logger.debug(s"Creating district #$district")
              val range = 1 to (if (district != totalDistricts) sensorQuantity / totalDistricts else sensorQuantity / totalDistricts + sensorQuantity % totalDistricts)
              val inputs = for (input <- range) yield "i" + input
              define anAdder() withInputs(inputs.map{e => e}:_*)
              flush()
              lastOperation.get.addProperty("name", "ADDER_DISTRICT_" + district)
              val sensors = (for (idx <- range) yield "PRK_" + ((district - 1) * (sensorQuantity / totalDistricts) + idx)).map {name => policy.findSensorByName(name).get}


              var input:Int = 1
              for (s <- sensors) {
                val flow = Flow(s.output.asInstanceOf[Output[SmartCampusType]],lastOperation.get.getInput("i" + input).asInstanceOf[Input[SmartCampusType]])
                input = input + 1
                policy = policy.addFlow(flow)
              }





            }
            if (totalDistricts == 1) {
              val adder = policy.concepts.collectFirst {case x:Add[_] => x}.get
              val collector = new Collector("DemoCollector", classOf[SmartCampusType])
              policy = policy.add(collector).add(Flow(adder.output, collector.input))
            }
            else {
              val aggregator = new Add((for (i <- 1 to totalDistricts) yield "i" + i).toSet,classOf[SmartCampusType])

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
            this.copy(districtQuantity = totalDistricts)
          }

          case class SmartParkingThresholdBuilder(builder: SmartParkingSensorBuilder) {
            def of(value: Int) = {
              val limit = 1 + (builder.sensorQuantity / builder.districtQuantity) * value / 100
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