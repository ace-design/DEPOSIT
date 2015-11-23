package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 23/11/2015.
  */
trait DEPOSIT {


  protected def hasForName(n:String) { policy.name = n }
  protected def hasForMappingFile(n:String) {mappingFile = Some(n)}

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

  /**********************
    ** Method chaining **
    *********************/

  protected object IOType extends Enumeration {
    val PERIODIC, EVENT, COLLECTOR, GENERIC_OUTPUT, GENERIC_INPUT, UNKNOWN = Value
  }

  protected case class IOBuilder(kind: IOType.Value = IOType.UNKNOWN, name: String = "", period: Option[Int] = None, dataType: Option[Class[_<:DataType]] = None) {

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
    val ADD, AVG, CONDITIONAL, CONSTANT, DIVIDE, HIGHER, HIGHEREQ, INCREMENT, LOWER, LOWEREQ, MAX, MIN, MULTIPLY, PRODUCE, SUB = Value
  }
  protected case class OperationBuilder(kind: OperationType.Value = IOType.UNKNOWN, inputs:Set[String] = Set.empty, outputs:Set[String] = Set.empty, rename:Option[String] = None, dataTypeInput: Option[Class[_<:DataType]] = None, dataTypeOutput:Option[Class[_<:DataType]] = None){

    def anAddOperation():OperationBuilder = {
      currentOperation = Some(this.copy(kind = OperationType.ADD))
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

    def handlingOnOutputs(t:Class[_<:DataType]): OperationBuilder = {
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
  protected var mappingFile:Option[String] = None

}
