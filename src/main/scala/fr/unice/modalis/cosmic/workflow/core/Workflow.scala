package fr.unice.modalis.cosmic.workflow.core

/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param activities Workflow element list
 * @param links Link list

 */
case class Workflow(val ios:Set[DataIO[_<:DataType]], val activities:Set[WFElement], val links:Set[WFLink]) {

  def this() = this(Set.empty, Set.empty, Set.empty)
  
  def addIO(o:DataIO[_<:DataType]):Workflow = {
    new Workflow(ios + o, activities, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity(c:WFActivity[_<:DataType,_<:DataType]):Workflow  = {
    new Workflow(ios, activities + c, links)
  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink):Workflow  = {
     new Workflow(ios, activities, links + l)
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink):Workflow  = {
    println("Delete [" + l + "]")
    new Workflow(ios, activities, links - l)
  }


  override def toString = "Workflow[elements={" + activities + "};links={" + links + "}]"
}
