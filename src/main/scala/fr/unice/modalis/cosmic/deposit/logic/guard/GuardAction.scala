package fr.unice.modalis.cosmic.actions.guard

/**
 * Guard trait
 * 05/13
 */
trait GuardAction {

  override def equals(x: Any) = x.toString.equals(this.toString)
}
