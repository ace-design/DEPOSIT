package fr.unice.modalis.cosmic.deposit.logic.guard.constraint

import fr.unice.modalis.cosmic.deposit.logic.guard.GuardAction


/**
 * Action constrain parent class
 */
trait Constraint extends GuardAction {
  override def toString: String
}
