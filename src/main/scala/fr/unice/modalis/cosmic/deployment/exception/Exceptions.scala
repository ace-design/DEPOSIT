package fr.unice.modalis.cosmic.deployment.exception

import fr.unice.modalis.cosmic.deposit.core.Concept

/**
 * Exceptions for the deployment process
 * Created by Cyril Cecchinel - I3S Laboratory on 19/06/15.
 */
case class NoTargetFoundException(c:Concept) extends Exception("No target found for " + c)
case class InappropriateConceptForGenerator(c:Concept, t:String) extends Exception(c.commonName + " can not be generated to " + t)
