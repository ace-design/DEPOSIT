package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deployment.heuristics.DeploymentHeuristic
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

  /***********************
    * Deployment process *
    **********************/

  def deploy() = {
    val infrastructure = InfrastructureModelBuilder(targetFile.getOrElse(throw new Exception("No mapping file provided")))
    val predeployed = PreDeploy(policy, infrastructure)
    Deploy.deploy(predeployed, infrastructure, DeploymentHeuristic.CLOSER_TO_THE_SENSORS)
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

  trait ConceptBuilder

  case class InterfaceBuilder(builder: ConceptBuilder, name: String, way: InterfaceType.Value) {
    def ->(other:InterfaceBuilder):FlowBuilder = FlowBuilder(this, other)
  }

  case class FlowBuilder(source: InterfaceBuilder, destination: InterfaceBuilder) {

    val link = (source.builder,destination.builder) match {
      case (a:IOBuilder,b:IOBuilder) => new Link(a.toIO.asInstanceOf[DataInput[_<:DataType]].output, b.toIO.asInstanceOf[DataOutput[_<:DataType]].input)
      case (a:IOBuilder,b:OperationBuilder) => new Link(a.toIO.asInstanceOf[DataInput[_<:DataType]].output, b.toOperation.getInput(destination.name))
      case (a:OperationBuilder, b:OperationBuilder) => new Link(a.toOperation.getOutput(source.name), b.toOperation.getInput(destination.name))
      case (a:OperationBuilder, b:IOBuilder) => new Link(a.toOperation.getOutput(source.name), b.toIO.asInstanceOf[DataOutput[_<:DataType]].input)
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
      case IOType.PERIODIC => new PeriodicSensor(period.get, name, dataType.get)
      case IOType.EVENT => new EventSensor(name, dataType.get)
      case IOType.COLLECTOR => new Collector(name, dataType.get)
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
                                        dataTypeInput: Option[Class[_<:DataType]] = None,
                                        dataTypeOutput:Option[Class[_<:DataType]] = None) extends ConceptBuilder{

    def apply(s:String):InterfaceBuilder = InterfaceBuilder(this, s, InterfaceType.OUTPUT)

    def aFilter(s:String):OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.CONDITIONAL, parameter = Some(s)))
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
      case OperationType.ADD => new Add(inputs, dataTypeInput.get, rename)
      case OperationType.CONDITIONAL => new Conditional(parameter.get, dataTypeInput.get)
      case OperationType.PRODUCE => new Produce(inputs, produceTrue.get, produceFalse, dataTypeInput.get.asInstanceOf[Class[DataType]], dataTypeOutput.get.asInstanceOf[Class[DataType]])
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
