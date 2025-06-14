package v2


import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.immutable
import scalax.collection.immutable.Graph
import v2.GlobalAuxiliaries.* 

/** Класс для представления тестируемой системы.
  *
  * @param graph
  *   3-partie graph with Signals, videoshots and workstations
  * @param signals
  *   set of signals
  * @param videoshots
  *   set of videoshots
  * @param workstations
  *   set of workstations
  * @param workstationDisplays
  *   map from workstation to number of displays it has
  */
case class SystemModel(
    graph: Graph[Int, WDiEdge[Int]],
    signals: Seq[Int],
    videoshots: Seq[Int],
    workstations: Seq[Int],
    workstationDisplays: Map[Int, Int],
    props:SystemModel.SystemModelProps
):
  override def toString(): String =
    showNodesSuccessors("Signals", graph, signals) +
      showNodesSuccessors("Videoshots", graph, videoshots) +
      showNodesSuccessors("Workstations", graph, workstations) +
      printCollection("Workstation Displays", workstationDisplays)
    
  
object SystemModel{
  case class SystemModelProps(signals: Int,
                              videoshots: Int,
                              workstations: Int,
                              signal2Shots: Int,
                              shot2Workstations: Int,
                              displayLimits: (Int, Int))
  val defProps: SystemModelProps = SystemModelProps(0,0,0,0,0,(0,0))
}