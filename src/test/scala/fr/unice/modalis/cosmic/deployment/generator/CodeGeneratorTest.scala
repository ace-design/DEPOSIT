package fr.unice.modalis.cosmic.deployment.generator

import java.io._

import fr.unice.modalis.cosmic.ComprehensivePolicyWithoutDSL
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.ProgrammingLanguage.ProgrammingLanguage
import fr.unice.modalis.cosmic.deployment.infrastructure.Features.{ProgrammingLanguage, SensorBrand, SensorType}
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
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

    override def generateConstant(s: DataType): String = ???

    override def generateInstruction[T <: SensorDataType, U <: SensorDataType](c: Concept, policy: Policy): Instruction = ???

    override def produceSourceFile(name: String, code: String): Unit = ???

    override val language: ProgrammingLanguage = ProgrammingLanguage.None
    override val sensorTypeHandling: HashMap[SensorType.Value, (String, String, String)] = HashMap()
    override val sensorBrandHandling: HashMap[SensorBrand.Value, (String, String)] = HashMap()

    override def generateDataStructure(dataType: DataType): String = ???

    override def generateIntraMessage(dataType: DataType): String = ???
  }

  "A code generator" should {
    "compute concept generation order" in {
      new GeneratorTest().orderedGenerationList(DCPTest.p2) must haveSize(DCPTest.p2.operations.size + DCPTest.p2.ios.size)
    }
  }

  "Processing code generator" should {
    "generate the code for a given policy" in {
      val testFile = new File("out/arduino/testFile.ino")
      val bw = new BufferedWriter(new FileWriter(testFile))
      val infra = TopologyModelBuilder("assets/configurations/smartcampus_xbeenetwork.xml")
      val policies = Deploy(PreDeploy(ComprehensivePolicyWithoutDSL.p, infra), infra, DeploymentRepartition.CLOSEST_TO_SENSORS)
      val candidate = policies.find(_.readProperty("board_type").get.asInstanceOf[String] equals "Arduino")
      bw.write(ProcessingGenerator(candidate.get))
      bw.close()

      testFile.length() mustNotEqual 0
      testFile.delete()
    }
  }

}
