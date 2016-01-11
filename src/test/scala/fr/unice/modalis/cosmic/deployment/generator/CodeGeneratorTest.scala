package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deployment.infrastructure.Features.ProgrammingLanguage.ProgrammingLanguage
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{ProgrammingLanguage, SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deposit.core.{Concept, DCPTest, Policy, SensorDataType}
import org.specs2.mutable.SpecificationWithJUnit

import scala.collection.immutable.HashMap

/**
 * Code generator test
 * Created by Cyril Cecchinel - I3S Laboratory on 13/10/2015.
 */
class CodeGeneratorTest  extends SpecificationWithJUnit{

  /**
   * Class used to test trait methods
   */
  class GeneratorTest extends CodeGenerator {
    override val templateFile: String = ""

    override def generateGlobalVariables(policy: Policy): String = ???

    override def generatePolicyBody(policy: Policy): String = ???

    override def generateDataStructures(p: Policy): String = ???

    override val CURRENT_TIMESTAMP_METHOD: String = ""

    override def generateInputs(policy: Policy): (String,String) = ???

    override def generateConstant(s: SensorDataType): String = ???

    override def generateInstruction[T <: SensorDataType, U <: SensorDataType](c: Concept, policy: Policy): Instruction = ???

    override def produceSourceFile(name: String, code: String): Unit = ???

    override val language: ProgrammingLanguage = ProgrammingLanguage.None
    override val sensorTypeHandling: HashMap[SensorType.Value, (String, String, String)] = HashMap()
    override val sensorBrandHandling: HashMap[SensorBrand.Value, (String, String)] = HashMap()
  }

  "A code generator" should {
    "compute concept generation order" in {
      new GeneratorTest().orderedGenerationList(DCPTest.p2) must haveSize(DCPTest.p2.operations.size + DCPTest.p2.ios.size)
    }
  }


}
