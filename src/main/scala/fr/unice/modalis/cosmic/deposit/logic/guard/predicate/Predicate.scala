package fr.unice.modalis.cosmic.deposit.logic.guard.predicate

import fr.unice.modalis.cosmic.deposit.logic.guard.GuardAction

/**
 * Created by cyrilcecchinel on 13/05/2014.
 */
trait Predicate extends GuardAction {
  override def toString: String

}
