package fr.unice.modalis.cosmic.actions.guard.predicate

import fr.unice.modalis.cosmic.actions.guard.GuardAction

/**
 * Created by cyrilcecchinel on 28/04/2014.
 */
case class NOTPredicate(val expression: GuardAction) extends Predicate {
  override def toString(): String = "NOT(" + expression + ")"

}
