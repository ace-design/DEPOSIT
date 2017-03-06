package fr.unice.modalis.cosmic.deployment.generator

import java.io._

import fr.unice.modalis.cosmic.ComprehensivePolicyWithoutDSL
import fr.unice.modalis.cosmic.deployment.strategies.DeploymentRepartition
import fr.unice.modalis.cosmic.deployment.utils.TopologyModelBuilder
import fr.unice.modalis.cosmic.deployment.{Deploy, PreDeploy}
import fr.unice.modalis.cosmic.deposit.core._
import org.specs2.mutable.SpecificationWithJUnit

/**
 * Code generator test
 * Created by Cyril Cecchinel - I3S Laboratory on 13/10/2015.
 */
class CodeGeneratorTest  extends SpecificationWithJUnit{

  "A code generator" should {
    "compute concept generation order" in {
      CodeGenerator.orderedGenerationList(DCPTest.p2) must haveSize(DCPTest.p2.operations.size + DCPTest.p2.ios.size)
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
