package fr.unice.modalis.cosmic.deposit.core

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 05/05/15.
 */
class NotExtendableException(p:Policy) extends Exception(p.name + " is not extendable")
