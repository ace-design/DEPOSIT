package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deposit.algo.{ExtendPolicy, Unification, Weave}
import fr.unice.modalis.cosmic.deposit.core._

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 11/04/2017.
  */
trait ReuseDSL extends DEPOSIT{

  def selectIn(policy:Policy):SelectBuilder = {
    currentSelection = Some(SelectBuilder(policy))
    currentSelection.get
  }

  def weaveBetween(policy1: Policy, policy2: Policy) = {
    currentWeave = Some(WeaveBuilder(policy1, policy2))
    currentWeave.get
  }

  case class SelectBuilder(_policy: Policy){
    def conceptsMarked(markers:String*) = {
      policy = _policy.select(_policy.operations.filter(_._marker.isDefined).filter(o => markers.contains(o._marker.get)).toSet, "select_" + _policy.name)
      policy
    }
  }

  case class WeaveBuilder(policy1: Policy, policy2: Policy){
    val exPolicy1 = ExtendPolicy(policy1)
    val exPolicy2 = ExtendPolicy(policy2)

    def andAssociates(associations:((String,String) , (String,String))*) = {
      val unifications = associations.map{ association =>
        val jpoutput = exPolicy1.outputJoinPoints.find(p => p.fromConceptOutput.name == association._1._2 && p.fromConceptOutput.parent.asInstanceOf[Operation[_,_]]._marker.get == association._1._1)
        val jpinput = exPolicy2.inputJoinPoints.find(p => p.toConceptInput.name == association._2._2 && p.toConceptInput.parent.asInstanceOf[Operation[_,_]]._marker.get == association._2._1)
        if (jpinput.isDefined && jpoutput.isDefined){
          Unification(jpoutput.get.asInstanceOf[JoinPointOutput[DataType]], jpinput.get.asInstanceOf[JoinPointInput[DataType]])
        } else throw new Exception("Join point not found")
      }
      policy = Weave(exPolicy1, exPolicy2, unifications.toSet)
      policy
    }
  }

  protected var currentSelection: Option[SelectBuilder] = None

  protected var currentWeave: Option[WeaveBuilder] = None
}
