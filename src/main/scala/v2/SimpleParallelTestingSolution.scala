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
import v2.ParallelSolutionAuxiliaries.* 

object SimpleParallelTestingSolution
    extends ProblemSolution[
      SystemModel,
      (Int, SystemModel),
      PreparedGraph,
      ProcessedGraph,
      Seq[Action],
      Iterable[Action]
    ] {
  override def decomposeProblem(
      domainSystemModel: SystemModel
  ): Seq[(Int, SystemModel)] =
    domainSystemModel.signals.map(s => (s, domainSystemModel))

  override def transformToCalculationModel(
      problemPart: (Int, SystemModel)
  ): PreparedGraph =
    val (signal, model) = problemPart
    val (g1, source) = addSource(Seq(signal), model.graph)
    val (g2, sink) = addSink(g1, model.workstations, model.workstationDisplays)
    PreparedGraph(
      Set(signal),
      source,
      sink,
      g2,
      model.workstations,
      model.workstationDisplays
    )

  override def calculateModel(
      caclucaltionInput: PreparedGraph
  ): ProcessedGraph =
    // Распаковка даннных
    val PreparedGraph(
      signals,
      source,
      sink,
      graph,
      workstations,
      workstationDisplays
    ) = caclucaltionInput

    // number of connections for each signal
    val signalsSutturation =
      signals.map(s => s -> (graph get s).diSuccessors.size).toMap

    def go(
        sigs: Seq[Int],
        g: Graph[Int, WDiEdge[Int]],
        ws: Seq[Seq[Int]],
        wsDisplays: Seq[Map[Int, Int]]
    ): LazyList[(Graph[Int, WDiEdge[Int]], Seq[Seq[Int]])] =
      val (g1, source) = addSource(sigs, g)
      val (g2, sink) = addSink(g1, ws.flatten, wsDisplays.flatten.toMap)
      val maxFlowGraph = removeSourceAndSink(
        fordFulkersonMaximumFlow(g2, source, sink),
        source,
        sink
      )
      // All edges are sutturated than we have a final flow version
      if (isSutturated(maxFlowGraph, sigs, signalsSutturation))
        LazyList((maxFlowGraph, ws))
      else
        // if not, we duplicate the workstations and recalculate the flow
        val (newG, newWs, newWsMapping) =
          duplicateWorkstations(g, workstations, workstationDisplays)
        (maxFlowGraph, ws) #:: go(
          sigs,
          newG,
          ws.appended(newWs),
          wsDisplays.appended(newWsMapping)
        )

    //   полный поток и коответствующее количество копий рабочих станций
    val (completeFlow, workstationBunches) =
      go(signals.toSeq, graph, Seq(workstations), Seq(workstationDisplays)).last
    ProcessedGraph(signals, completeFlow, workstationBunches)

  override def interpreteModel(calculationResult: ProcessedGraph): Seq[Action] =
    // Распаковка даннных
    val ProcessedGraph(signals, completeFlow, workstationBunches) =
      calculationResult

    // рёбра отсортированные в по шагам
    val actionsEdges = workstationBunches.map {
      _.flatMap { worstation =>
        (completeFlow get worstation).edges
          .map(_.outer)
          .filter(e => e.target == worstation && e.weight > 0)
      }
    }

    // итог: действия
    val actions = actionsEdges.map { edges =>
      Action(
        signals.toSet,
        edges.map(e => e.source -> e.target).toMap
      )
    }
    actions

  override def agregateResults(
    domainResults: Iterable[Seq[Action]]
    ): Iterable[Action] = domainResults.flatten
}