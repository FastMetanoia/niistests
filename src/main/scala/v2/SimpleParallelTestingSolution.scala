package v2


import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.immutable
import scalax.collection.immutable.Graph
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
                               calculationInput: PreparedGraph
  ): ProcessedGraph =
    // Распаковка данных
    val PreparedGraph(
      signals,
      source,
      sink,
      graph,
      workstations,
      workstationDisplays
    ) = calculationInput

    // number of connections for each signal
    val signalsSaturation =
      signals.map(s => s -> (graph get s).diSuccessors.size).toMap

    def go(
            signals: Seq[Int],
            g: Graph[Int, WDiEdge[Int]],
            ws: Seq[Seq[Int]],
            wsDisplays: Seq[Map[Int, Int]]
    ): LazyList[(Graph[Int, WDiEdge[Int]], Seq[Seq[Int]])] =
      //val (g1, source) = addSource(signals, g)
      //val (g2, sink) = addSink(g1, ws.flatten, wsDisplays.flatten.toMap)
      val maxFlowGraph = jMaxFlow(g, source, sink)
//      println(showSystemGraph(maxFlowGraph,signals))
      // All edges are saturated than we have a final flow version
      if (isSaturated(maxFlowGraph, signals, signalsSaturation))
        LazyList((maxFlowGraph, ws))
      else
        // if not, we duplicate the workstations and recalculate the flow
        val (newG, newWs, newWsMapping) =
          duplicateWorkstations(g, workstations, workstationDisplays)
        (maxFlowGraph, ws) #:: go(
          signals,
          newG,
          ws.appended(newWs),
          wsDisplays.appended(newWsMapping)
        )

    //   полный поток и cоответствующее количество копий рабочих станций
    val (completeFlow, workstationBunches) =
      go(signals.toSeq, graph, Seq(workstations), Seq(workstationDisplays)).last
    ProcessedGraph(signals, completeFlow, workstations, workstationBunches)

  override def interpretModel(calculationResult: ProcessedGraph): Seq[Action] =
    // Распаковка данных
    val ProcessedGraph(signals, completeFlow, originalWorkstations, workstationBunches) =
      calculationResult

    def cycledWorkstations(workstations:Seq[Int], i:Int = 0):LazyList[Int] =
      workstations(i) #:: cycledWorkstations(workstations, (i+1) % workstations.length)

    val cycledOriginalWorkstations = cycledWorkstations(originalWorkstations)

    // рёбра отсортированные в по шагам
    val actionsEdges = workstationBunches.map { bunch=>
      bunch.zip(cycledOriginalWorkstations).flatMap { (copy, original) =>
        (completeFlow get copy).edges
          .map(_.outer)
          .filter(e => e.target == copy && e.weight > 0)
          .map(e=>(e, original))
      }
    }

    // итог: действия
    val actions = actionsEdges.map { edges =>
      Action(
        signals,
        edges.map((e, original) => e.source -> original).toMap
      )
    }
    actions

  override def aggregateResults(
    domainResults: Iterable[Seq[Action]]
    ): Iterable[Action] = domainResults.flatten
}