package fr.unice.modalis.cosmic.workflow.algo

import fr.unice.modalis.cosmic.workflow.core._

/**
 * Verify functions to check if a workflow is a correct one
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
object Verify {

  /**
   * Check if a link is valid
   * Authorized links
   * Sensor --> Getter
   * Getter --> Predicate
   * Predicate --> Collector
   * Predicate --> Predicate
   *
   * @param l Link
   * @return Link validity status
   */
def checkLink(l:WFLink[_]):Boolean = (l.source.component, l.destination.component) match {
  case (Source(_), PeriodicGetter(_)) => true
  case (PeriodicGetter(_), Predicate(_)) => true
  case (Predicate(_), Predicate(_)) => true
  case (Predicate(_), Sink(_)) => true
  case _ => false
}





}
