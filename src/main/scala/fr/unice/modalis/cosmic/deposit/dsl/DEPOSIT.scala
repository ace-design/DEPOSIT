package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core.DataField.DataField
import fr.unice.modalis.cosmic.deposit.core._

/**
  * DEPOSIT Language
  * Created by Cyril Cecchinel - I3S Laboratory on 23/11/2015.
  */
trait DEPOSIT {

  def apply() = this.policy

  protected def hasForName(n:String) { policy.name = n }
  protected def uses(n:String) { associationFile = Some(n)}
  protected def targets(n:String) {targetFile = Some(n)}
  protected def handles(c:Class[_<:SensorDataType]) {defaultType = Some(c)}

  protected def exportToWiring() = policy.exportToWiring
  protected def exportToPython() = policy.exportToPython
  protected def exportToGraphviz() = policy.exportToGraphviz

  /***********************
    * Deployment process *
    **********************/

  def deploy() = {
    val infrastructure = TopologyModelBuilder(targetFile.getOrElse(throw new Exception("No mapping file provided")))
    val predeployed = PreDeploy(policy, infrastructure)
    Deploy.deploy(predeployed, infrastructure, DeploymentRepartition.CLOSEST_TO_SENSORS)
  }
  /**************************
    * Top level declaration *
    *************************/

  protected def declare: IOBuilder = {
    flush()
    currentIO = Some(IOBuilder())
    currentIO.get

  }

  protected def define: OperationBuilder = {
    flush()
    currentOperation = Some(OperationBuilder())
    currentOperation.get
  }

  protected def flows(flowSystem : => Unit): Unit = {
    flush()
    flowSystem
  }

  /**********************
    ** Method chaining **
    *********************/

  protected object InterfaceType extends Enumeration {
    val INPUT, OUTPUT = Value
  }

  protected object IOType extends Enumeration {
    val PERIODIC, EVENT, COLLECTOR, GENERIC_OUTPUT, GENERIC_INPUT, UNKNOWN = Value
  }

  trait ConceptBuilder {
    var conceptProduced:Option[Concept] = None
  }

  case class InterfaceBuilder(builder: ConceptBuilder, name: String, way: InterfaceType.Value) {
    def ->(other:InterfaceBuilder):FlowBuilder = FlowBuilder(this, other)
  }

  case class FlowBuilder(source: InterfaceBuilder, destination: InterfaceBuilder) {

    val flow = (source.builder,destination.builder) match {
      case (a:IOBuilder,b:IOBuilder) => new Flow(a.conceptProduced.get.asInstanceOf[DataInput[_<:DataType]].output, b.conceptProduced.get.asInstanceOf[DataOutput[_<:DataType]].input)
      case (a:IOBuilder,b:OperationBuilder) => if (destination.name.equals("DEFAULT"))
        new Flow(a.conceptProduced.get.asInstanceOf[DataInput[_<:DataType]].output, b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput())
        else new Flow(a.conceptProduced.get.asInstanceOf[DataInput[_<:DataType]].output, b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(destination.name))
      case (a:OperationBuilder, b:OperationBuilder) => {
        (source.name, destination.name) match {
          case ("DEFAULT", "DEFAULT") => new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(), b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput())
          case ("DEFAULT", _) => new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(), b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(destination.name))
          case (_, "DEFAULT") => new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(source.name), b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput())
          case (_, _) => new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(source.name), b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(destination.name))
        }
      }
      case (a:OperationBuilder, b:IOBuilder) => if (source.name.equals("DEFAULT"))
        new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(), b.conceptProduced.get.asInstanceOf[DataOutput[_<:DataType]].input)
        else new Flow(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(source.name), b.conceptProduced.get.asInstanceOf[DataOutput[_<:DataType]].input)
    }
    policy = policy.copy(flows = policy.flows + flow)

  }


  protected case class IOBuilder(kind: IOType.Value = IOType.UNKNOWN, name: String = "", period: Option[Int] = None, dataType: Option[Class[_<:DataType]] = defaultType, marker:Option[String] = None) extends ConceptBuilder{
    def apply():InterfaceBuilder = kind match {
      case IOType.COLLECTOR | IOType.GENERIC_INPUT => InterfaceBuilder(this, name, InterfaceType.INPUT)
      case _ => InterfaceBuilder(this, name, InterfaceType.OUTPUT)
    }

    def aGenericInput(): IOBuilder = {
      currentIO = Some(this.copy(kind = IOType.GENERIC_INPUT))
      currentIO.get
    }

    def aGenericOutput():IOBuilder = {
      currentIO = Some(this.copy(kind = IOType.GENERIC_OUTPUT))
      currentIO.get
    }

    def aPeriodicSensor(): IOBuilder = {
      currentIO = Some(this.copy(kind = IOType.PERIODIC))
      currentIO.get
    }

    def anEventSensor(): IOBuilder = {
      currentIO = Some(this.copy(kind = IOType.EVENT))
      currentIO.get
    }

    def aCollector(): IOBuilder = {
      currentIO = Some(this.copy(kind = IOType.COLLECTOR))
      currentIO.get
    }

    def named(n:String): IOBuilder = {
      currentIO = Some(this.copy(name = n))
      currentIO.get
    }

    def withPeriod(p:Int): IOBuilder = {
      currentIO = Some(this.copy(period = Some(p)))
      currentIO.get
    }

    def handling(t:Class[_<:DataType]) = {
      currentIO = Some(this.copy(dataType = Some(t)))
      currentIO.get
    }

    /***********
      * REUSE
      */

    def withMarker(s:String = ""):IOBuilder = {
      if (s.equals(""))
        currentIO = Some(this.copy(marker = Some(this.kind.toString.toLowerCase())))
      else
        currentIO = Some(this.copy(marker = Some(s)))

      currentIO.get
    }

    def toIO = {
      val result = kind match {
        case IOType.PERIODIC => val c = new PeriodicSensor(period.get, name, dataType.get); conceptProduced = Some(c); c;
        case IOType.EVENT => val c = new EventSensor(name, dataType.get); conceptProduced = Some(c); c;
        case IOType.COLLECTOR => val c = new Collector(name, dataType.get); conceptProduced = Some(c); c;
        case IOType.GENERIC_INPUT => val c = new GenericInput(name, dataType.get); conceptProduced = Some(c); c;
        case IOType.GENERIC_OUTPUT => val c = new GenericOutput(name, dataType.get); conceptProduced = Some(c); c;
      }
      if (marker.isDefined) result.setMarker(marker.get)
      result
    }



  }

  protected object OperationType extends Enumeration {
    val ABS, ADD, AVG, CONDITIONAL, CONSTANT, DIVIDE, HIGHER, HIGHEREQ, INCREMENT, LOWER, LOWEREQ, MAX, MIN, MULTIPLY, PRODUCE, PROCESS, RENAME, SUB, UNKNOWN = Value
  }
  protected case class OperationBuilder(kind: OperationType.Value = OperationType.UNKNOWN,
                                        inputs:Set[String] = Set.empty,
                                        outputs:Set[String] = Set.empty,
                                        rename:Option[String] = None,
                                        parameter:Option[String] = None,
                                        produceTrue:Option[SensorDataType] = None,
                                        produceFalse:Option[SensorDataType] = None,
                                        atomicValue:Option[AtomicType] = None,
                                        innerPolicy:Option[Policy] = None,
                                        dataTypeInput: Option[Class[_<:DataType]] = defaultType,
                                        dataTypeOutput:Option[Class[_<:DataType]] = defaultType,
                                        marker:Option[String] = None,
                                        applicationField:Option[DataField] = Some(DataField.OBSERVATION)) extends ConceptBuilder{

    def apply(s:String):InterfaceBuilder = InterfaceBuilder(this, s, InterfaceType.OUTPUT)
    def apply():InterfaceBuilder = InterfaceBuilder(this, "DEFAULT", InterfaceType.OUTPUT)

    /** PROXY **/
    def aCondition(s:String) = aFilter(s)
    /****/

    def anAbsoluteValue():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.ABS))
      currentOperation.get
    }

    def anAvg():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.AVG))
      currentOperation.get
    }

    def aFilter(s:String):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.CONDITIONAL, parameter = Some(s)))
      currentOperation.get
    }

    def aDividerBy(s:AtomicType):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.DIVIDE, atomicValue = Some(s)))
      currentOperation.get
    }

    def aMultiplyBy(s:AtomicType):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.MULTIPLY, atomicValue = Some(s)))
      currentOperation.get
    }

    def anIncrementBy(s:AtomicType):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.INCREMENT, atomicValue = Some(s)))
      currentOperation.get
    }

    def anAdder():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.ADD))
      currentOperation.get
    }

    def aSubstractor():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.SUB))
      currentOperation.get
    }

    def aProducer(t:SensorDataType, s:Option[SensorDataType] = None) = {
      currentOperation = Some(this.copy(kind = OperationType.PRODUCE, produceTrue = Some(t), produceFalse = s))
      currentOperation.get
    }

    def aMax():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.MAX))
      currentOperation.get
    }

    def aProcess(p:Policy) = {
      currentOperation = Some(this.copy(kind = OperationType.PROCESS, innerPolicy = Some(p)))
      currentOperation.get
    }

    def aRenamerTo(s:String) = {
      currentOperation = Some(this.copy(kind = OperationType.RENAME, rename = Some(s)))
      currentOperation.get
    }

    def withInputs(i:String *):OperationBuilder = {
      currentOperation = Some(this.copy(inputs = i.toSet))
      currentOperation.get
    }

    def withOutputs(o:String *):OperationBuilder = {
      currentOperation = Some(this.copy(outputs = o.toSet))
      currentOperation.get
    }

    def handingOnInputs(t:Class[_<:DataType]): OperationBuilder = {
      currentOperation = Some(this.copy(dataTypeInput = Some(t)))
      currentOperation.get
    }

    def andHandlingOnOutputs(t:Class[_<:DataType]): OperationBuilder = {
      currentOperation = Some(this.copy(dataTypeOutput = Some(t)))
      currentOperation.get
    }

    def handling(t:Class[_<:DataType]): OperationBuilder = {
      currentOperation = Some(this.copy(dataTypeInput = Some(t), dataTypeOutput = Some(t)))
      currentOperation.get
    }



    def andRenameData(s:String): OperationBuilder = {
      currentOperation = Some(this.copy(rename = Some(s)))
      currentOperation.get
    }

    def onField(field:DataField):OperationBuilder = {
      currentOperation = Some(this.copy(applicationField = Some(field)))
      currentOperation.get
    }

    def toOperation = {
      val result = kind match {
        case OperationType.ABS => val c = new Abs(dataTypeInput.get, rename); conceptProduced = Some(c); c;
        case OperationType.ADD => val c = new Add(inputs, dataTypeInput.get, rename); conceptProduced = Some(c); c;
        case OperationType.AVG => val c = new Average(inputs, dataTypeInput.get, rename); conceptProduced = Some(c); c;
        case OperationType.CONDITIONAL => val c = new Conditional(parameter.get, dataTypeInput.get, applicationField.get); conceptProduced = Some(c); c;
        case OperationType.PRODUCE => val c = new Produce(inputs, produceTrue.get, produceFalse, dataTypeInput.get.asInstanceOf[Class[DataType]], dataTypeOutput.get.asInstanceOf[Class[DataType]]); conceptProduced = Some(c); c;
        case OperationType.DIVIDE => val c = new Divide(atomicValue.get, dataTypeInput.get); conceptProduced = Some(c); c;
        case OperationType.MULTIPLY => val c = new Multiply(atomicValue.get, dataTypeInput.get); conceptProduced = Some(c); c;
        case OperationType.MAX => val c = new Max(inputs, dataTypeInput.get, applicationField.get); conceptProduced = Some(c); c;
        case OperationType.INCREMENT => val c = new Increment(atomicValue.get, dataTypeInput.get); conceptProduced = Some(c); c;
        case OperationType.RENAME => val c = new Rename(rename.get, dataTypeInput.get); conceptProduced = Some(c); c;
        case OperationType.PROCESS => val c = new Process(innerPolicy.get, dataTypeInput.get, dataTypeOutput.get); conceptProduced = Some(c); c;
        case OperationType.SUB => val c = new Sub(inputs, dataTypeInput.get, rename); conceptProduced = Some(c); c
      }
      if (marker.isDefined) result.setMarker(marker.get)
      result
    }

    /***********
      * REUSE
      */

    def withMarker(s:String = ""):OperationBuilder = {
      if (s.equals(""))
        currentOperation = Some(this.copy(marker = Some(this.kind.toString.toLowerCase())))
      else
        currentOperation = Some(this.copy(marker = Some(s)))

      currentOperation.get
    }
  }


  /*********************
    ** Private helpers **
    ********************/
  protected def flush(): Unit = {
    if (currentIO.isDefined) {
      policy = policy.copy(ios = policy.ios + currentIO.get.toIO)
      currentIO = None
    }

    if (currentOperation.isDefined) {
      val operation = currentOperation.get.toOperation
      policy = policy.copy(operations = policy.operations + operation)
      lastOperation = Some(operation)
      currentOperation = None
    }

  }

  protected var policy = new Policy()
  protected var currentIO : Option[IOBuilder] = None
  protected var currentOperation: Option[OperationBuilder] = None
  protected var currentFlow: Option[FlowBuilder] = None
  protected var defaultType:Option[Class[_<:SensorDataType]] = None
  protected var associationFile:Option[String] = None
  protected var targetFile:Option[String] = None
  protected var lastOperation:Option[Operation[_,_]] = None



}
