package fr.unice.modalis.cosmic.deployment.generator

import fr.unice.modalis.cosmic.deposit.core.{DCPTest, Policy}
import org.specs2.mutable.SpecificationWithJUnit

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
  }

  "A code generator" should {
    "compute concept generation order" in {
      new GeneratorTest().orderedGenerationList(DCPTest.p2) must haveSize(DCPTest.p2.operations.size + DCPTest.p2.ios.size)
    }
  }


}
