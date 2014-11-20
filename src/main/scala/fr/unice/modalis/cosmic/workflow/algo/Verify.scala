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

  /**
   * Verify the connectivity of a workflow (assumed as a directed graph)
   * Implements algorithm : http://fr.wikipedia.org/wiki/Algorithme_de_parcours_en_largeur
   * @param wf Workflow
   * @tparam T Workflow data type
   * @return Connectivity of the workflow
   */
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


  /**
   * Check if sinks are collectors
   * @param wf Workflow
   * @tparam T Workflow data type
   * @return All sinks are collectors
   */
  def collectorSink[T<:DataType](wf:Workflow[T]) = {

    // Destination of all links is the source of an other link.
    var res = Set.empty[WFLink[T]]
    for (i <- wf.links.iterator) {
      res = res ++  wf.links.filter(p => (p.destination.parent == i.source.parent))
    }

    // Get elements from the links
    val remaining = res.foldLeft(Set.empty[WFElement[T]]){(acc, e) => acc ++ Set(e.source.parent, e.destination.parent)}

    // The only remaining elements must be collectors
    val result = wf.elements -- remaining

    result.forall(p=> p match {case Sink(_) => true; case _ => false})
  }


}
