package fr.unice.modalis.cosmic.deposit.core

/**
 * Exceptions related to policies
 * Created by Cyril Cecchinel - I3S Laboratory on 05/05/15.
 */
case class NotExpendableException(p:Policy) extends Exception(p.name + " is not expendable")
case class PortNotFoundException(s:String) extends Exception(s + " does not exist")
case class UnknownDataTypeName(s:String) extends Exception(s + " is an unknown data type")