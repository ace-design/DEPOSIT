package fr.unice.modalis.cosmic.workflow.core


/**
 * Workflow definition
 * Created by Cyril Cecchinel - I3S Laboratory on 03/11/14.
 * @param activities Workflow element list
 * @param links Link list

 */
case class Workflow(val name:String, val ios:Set[DataIO[_<:DataType]], val activities:Set[WFActivity[_<:DataType,_<:DataType]], val links:Set[WFLink[_<:DataType]]) {

  def this(name:String) = this(name, Set.empty, Set.empty, Set.empty)
  def this() = this("wf" + scala.util.Random.alphanumeric.take(5).mkString, Set.empty, Set.empty, Set.empty)

  lazy val sources = ios.filter(_.isInstanceOf[Sensor[_<:DataType]]).asInstanceOf[Set[Sensor[_<:DataType]]]
  lazy val collectors = ios.filter(_.isInstanceOf[Collector[_<:DataType]]).asInstanceOf[Set[Collector[_<:DataType]]]

  def addIO(o:DataIO[_<:DataType]):Workflow = {
    new Workflow(name, ios + o, activities, links)
  }
  /**
   * Add an element in the current workflow
   * @param c Workflow Element
   * @return A new workflow with the element added
   */
  def addActivity(c:WFActivity[_<:DataType,_<:DataType]):Workflow  = {
    new Workflow(name, ios, activities + c, links)
  }

  /**
   * Add a link in the current workflow
   * @param l Link
   * @return A new workflow with the link added
   */
  def addLink(l:WFLink[_<:DataType]):Workflow  = {
     new Workflow(name, ios, activities, links + l)
  }

  /**
   * Delete an activity in the current workflow (/!\ Delete also all links referring this activity)
   * @param c Workflow activity
   * @return A workflow without this activity and links referring this activity
   */
  def deleteActivity(c:WFActivity[_<:DataType,_<:DataType]):Workflow = {
    new Workflow(name, ios, activities - c, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Delete an IO in the current workflow (/!\ Delete also all links referring this IO)
   * @param c Workflow IO
   * @return A workflow without this IO and links referring this IO
   */
  def deleteIO(c:DataIO[_<:DataType]):Workflow = {
    new Workflow(name, ios - c, activities, links.filterNot(p => (p.destination == c) || (p.source == c)))
  }

  /**
   * Delete a link in the current workflow
   * @param l Link
   * @return A workflow with the linked removed
   */
  def deleteLink(l:WFLink[_<:DataType]):Workflow  = {
    println("Delete [" + l + "]")
    new Workflow(name, ios, activities, links - l)
  }


  override def toString = "Workflow[name=" + name + ";ios={" + ios + "};activites={" + activities + "};links={" + links + "}]"
}
