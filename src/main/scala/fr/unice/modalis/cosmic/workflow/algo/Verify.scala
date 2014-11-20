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
def checkLink[T<:DataType](l:WFLink[T]):Boolean = (l.source.parent, l.destination.parent) match {
  case (Source(_), PeriodicGetter(_)) => true
  case (PeriodicGetter(_), Predicate(_)) => true
  case (Predicate(_), Predicate(_)) => true
  case (Predicate(_), Sink(_)) => true
  case _ => false
}

  def connectivity[T<:DataType](wf:Workflow[T]) = {

    def BFS[T<:DataType](l:List[WFLink[T]], s:Source[T]) = {
      val f = scala.collection.mutable.Queue[WFElement[T]]()
      var visited = List[WFElement[T]]()
      f.enqueue(s)
      visited = s :: visited
      while (f.size > 0){
        val current = f.dequeue()
        l.filter(link => link.source.parent == current).foreach(
          e => if (!visited.contains(e.destination.parent)){
            f.enqueue(e.destination.parent)
            visited = current :: visited
          }
        )
      }

      visited
    }

    BFS(wf.links.toList, wf.sources.head).size == wf.elements.size
  }






}
