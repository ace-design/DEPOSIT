package fr.unice.modalis.cosmic.deposit.dsl

import fr.unice.modalis.cosmic.deposit.core.Policy

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 11/04/2017.
  */
trait ReuseDSL {

  def selectIn(policy:Policy):SelectBuilder = {
    currentSelection = Some(SelectBuilder(policy))
    currentSelection.get
  }

  case class SelectBuilder(policy: Policy){
    def conceptsMarked(markers:String*) = {
      policy.select(policy.operations.filter(_._marker.isDefined).filter(o => markers.contains(o._marker.get)).toSet, "select_" + policy.name)
    }
  }

  protected var currentSelection: Option[SelectBuilder] = None
}
