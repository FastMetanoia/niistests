package v2


import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import scalax.collection.mutable
import scalax.collection.immutable
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import scalax.collection.edges.labeled.:~>
import scalax.collection.edges.labeled.%
import scalax.collection.GraphOps
import scala.collection.IndexedSeqView.Id
import scalax.collection.edges.UnDiEdge
import scalax.collection.AnyGraph
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
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
    workstationDisplays: Map[Int, Int]
):
  override def toString(): String =
    showNodesSuccessors("Signals", graph, signals) +
      showNodesSuccessors("Videoshots", graph, videoshots) +
      showNodesSuccessors("Workstations", graph, workstations) +
      printCollection("Workstation Displays", workstationDisplays)