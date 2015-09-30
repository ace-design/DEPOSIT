package fr.unice.modalis.cosmic.deposit.core

/**
 * Exceptions related to policies
 * Created by Cyril Cecchinel - I3S Laboratory on 05/05/15.
 */
class NotExpendableException(p:Policy) extends Exception(p.name + " is not expendable")
