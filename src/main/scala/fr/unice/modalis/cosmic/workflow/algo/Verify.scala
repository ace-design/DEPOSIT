package fr.unice.modalis.cosmic.workflow.algo


import fr.unice.modalis.cosmic.workflow.core._

/**
 * Verify functions to check if a workflow is a correct one
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 */
object Verify {


  /**
   * Verify the connectivity of a workflow (assumed as a directed graph)
   * Implements algorithm : http://fr.wikipedia.org/wiki/Algorithme_de_parcours_en_largeur
   * @param wf Workflow
   * @return Connectivity of the workflow
   */
  def connectivity(wf:Workflow) = {

    def BFS(l:List[WFLink], s:Source[DataType]) = {
      val f = scala.collection.mutable.Queue[WFElement]()
      var visited = List[WFElement]()
      f.enqueue(s)
      visited = s :: visited
      while (f.size > 0){
        val current = f.dequeue()
        l.filter(link => link.source == current).foreach(
          e => if (!visited.contains(e.destination)){
            f.enqueue(e.destination)
            visited = current :: visited
          }
        )
      }

      visited
    }
    wf.sources.forall(s => BFS(wf.links.toList, s).size == (wf.elements.size - wf.sources.filterNot(_ == s).size))
  }


  /**
   * Check if sinks are collectors
   * @param wf Workflow
   * @return All sinks are collectors
   */
  def collectorSink(wf:Workflow) = {

    // Destination of all links is the source of an other link.
    var res = Set.empty[WFLink]
    for (i <- wf.links.iterator) {
      res = res ++  wf.links.filter(p => (p.destination == i.source))
    }

    // Get elements from the links
    val remaining = res.foldLeft(Set.empty[WFElement]){(acc, e) => acc ++ Set(e.source, e.destination)}

    // The only remaining elements must be collectors
    val result = wf.elements -- remaining

    result.forall(p=> p match {case Sink(_) => true; case _ => false})
  }


}
