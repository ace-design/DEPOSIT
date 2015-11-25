package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deployment.heuristics.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.InfrastructureModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 23/11/2015.
  */
trait DEPOSIT {


  protected def hasForName(n:String) { policy.name = n }
  protected def uses(n:String) { associationFile = Some(n)}
  protected def targets(n:String) {targetFile = Some(n)}


  protected def exportToWiring() = policy.exportToWiring
  protected def exportToPython() = policy.exportToPython
  protected def exportToGraphviz() = policy.exportToGraphviz

  /***********************
    * Deployment process *
    **********************/

  def deploy() = {
    val infrastructure = InfrastructureModelBuilder(targetFile.getOrElse(throw new Exception("No mapping file provided")))
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

    val link = (source.builder,destination.builder) match {
      case (a:IOBuilder,b:IOBuilder) => new Link(a.conceptProduced.get.asInstanceOf[DataInput[_<:DataType]].output, b.conceptProduced.get.asInstanceOf[DataOutput[_<:DataType]].input)
      case (a:IOBuilder,b:OperationBuilder) => new Link(a.conceptProduced.get.asInstanceOf[DataInput[_<:DataType]].output, b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(destination.name))
      case (a:OperationBuilder, b:OperationBuilder) => new Link(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(source.name), b.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getInput(destination.name))
      case (a:OperationBuilder, b:IOBuilder) => new Link(a.conceptProduced.get.asInstanceOf[Operation[_<:DataType,_<:DataType]].getOutput(source.name), b.conceptProduced.get.asInstanceOf[DataOutput[_<:DataType]].input)
    }
    policy = policy.copy(links = policy.links + link)

  }


  protected case class IOBuilder(kind: IOType.Value = IOType.UNKNOWN, name: String = "", period: Option[Int] = None, dataType: Option[Class[_<:DataType]] = None) extends ConceptBuilder{
    def apply():InterfaceBuilder = kind match {
      case IOType.COLLECTOR | IOType.GENERIC_INPUT => InterfaceBuilder(this, name, InterfaceType.INPUT)
      case _ => InterfaceBuilder(this, name, InterfaceType.OUTPUT)
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

    def toIO = kind match {
      case IOType.PERIODIC => val c = new PeriodicSensor(period.get, name, dataType.get); conceptProduced = Some(c); c;
      case IOType.EVENT => val c = new EventSensor(name, dataType.get); conceptProduced = Some(c); c;
      case IOType.COLLECTOR => val c = new Collector(name, dataType.get); conceptProduced = Some(c); c;
    }

  }

  protected object OperationType extends Enumeration {
    val ADD, AVG, CONDITIONAL, CONSTANT, DIVIDE, HIGHER, HIGHEREQ, INCREMENT, LOWER, LOWEREQ, MAX, MIN, MULTIPLY, PRODUCE, SUB, UNKNOWN = Value
  }
  protected case class OperationBuilder(kind: OperationType.Value = OperationType.UNKNOWN,
                                        inputs:Set[String] = Set.empty,
                                        outputs:Set[String] = Set.empty,
                                        rename:Option[String] = None,
                                        parameter:Option[String] = None,
                                        produceTrue:Option[SensorDataType] = None,
                                        produceFalse:Option[SensorDataType] = None,
                                        atomicValue:Option[AtomicType] = None,
                                        dataTypeInput: Option[Class[_<:DataType]] = None,
                                        dataTypeOutput:Option[Class[_<:DataType]] = None) extends ConceptBuilder{

    def apply(s:String):InterfaceBuilder = InterfaceBuilder(this, s, InterfaceType.OUTPUT)

    def aFilter(s:String):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.CONDITIONAL, parameter = Some(s)))
      currentOperation.get
    }

    def aDividerBy(s:AtomicType):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.DIVIDE, atomicValue = Some(s)))
      currentOperation.get
    }
    def anAdder():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.ADD))
      currentOperation.get
    }

    def aProducer(t:SensorDataType, s:Option[SensorDataType] = None) = {
      currentOperation = Some(this.copy(kind = OperationType.PRODUCE, produceTrue = Some(t), produceFalse = s))
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

    def toOperation = kind match {
      case OperationType.ADD => val c = new Add(inputs, dataTypeInput.get, rename); conceptProduced = Some(c); c;
      case OperationType.CONDITIONAL => val c = new Conditional(parameter.get, dataTypeInput.get); conceptProduced = Some(c); c;
      case OperationType.PRODUCE => val c = new Produce(inputs, produceTrue.get, produceFalse, dataTypeInput.get.asInstanceOf[Class[DataType]], dataTypeOutput.get.asInstanceOf[Class[DataType]]); conceptProduced = Some(c); c;
      case OperationType.DIVIDE => val c = new Divide(atomicValue.get, dataTypeInput.get); conceptProduced = Some(c); c;
    }
  }


  /*********************
    ** Private helpers **
    ********************/
  private def flush(): Unit = {
    if (currentIO.isDefined) {
      policy = policy.copy(ios = policy.ios + currentIO.get.toIO)
      currentIO = None
    }

    if (currentOperation.isDefined) {
      policy = policy.copy(operations = policy.operations + currentOperation.get.toOperation)
      currentOperation = None
    }

  }

  protected var policy = new Policy()
  protected var currentIO : Option[IOBuilder] = None
  protected var currentOperation: Option[OperationBuilder] = None
  protected var currentFlow: Option[FlowBuilder] = None

  protected var associationFile:Option[String] = None
  protected var targetFile:Option[String] = None



}
