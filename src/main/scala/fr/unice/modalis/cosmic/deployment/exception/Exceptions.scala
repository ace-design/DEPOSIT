package fr.unice.modalis.cosmic.deployment.exception

import fr.unice.modalis.cosmic.deposit.core.Operation

/**
 * Exceptions for the deployment process
 * Created by Cyril Cecchinel - I3S Laboratory on 19/06/15.
 */
class NoTargetFoundException(o:Operation[_,_]) extends Exception("No target found for " + o)
