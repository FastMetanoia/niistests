package v2


import scalax.collection.edges.{UnDiEdge, UnDiEdgeImplicits, ~}
import scalax.collection.edges.labeled.{WDiEdge, WDiEdgeFactory}
import scalax.collection.immutable
import scalax.collection.immutable.Graph
import v2.GlobalAuxiliaries.{addSink, addSource, printCollection, showSystemGraph}
import v2.ParallelSolutionAuxiliaries.*

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.language.postfixOps

type Clique = Set[Int]
type DomainSystemModel = SystemModel
type DomainProblemPart = (Clique, SystemModel)
type CalculationModelInput = PreparedGraph
type CalculationModelOutput = ProcessedGraph
type DomainSolutionPart = Iterable[Action]
type DomainSolutionWhole = Iterable[Action]

object CliqueParallelTestingSolution extends ProblemSolution[
  DomainSystemModel,
  DomainProblemPart,
  CalculationModelInput,
  CalculationModelOutput,
  DomainSolutionPart,
  DomainSolutionWhole
] {
  override def decomposeProblem(domainSystemModel: DomainSystemModel): Iterable[DomainProblemPart] = {
    val signals = domainSystemModel.signals
    val systemGraph = domainSystemModel.graph

    // Построение графа изоляции. Т. е. графа, в котором вершины - это сигналы, а ребро между 2 сигналами существует,
    // тогда и только тогда, когда эти 2 сигнала изолированы.

    //println(domainSystemModel)

    // изоляция в контексте решаемой задачи означает отсутствие пересечений множеств смежных видеокадров.
    val isolationGraph: Graph[Int, UnDiEdge[Int]] = {
      val edges = for {
        s1 <- signals
        s2 <- signals
        n1 = systemGraph get s1
        n2 = systemGraph get s2
        if n1.diSuccessors.intersect(n2.diSuccessors).isEmpty
      } yield s1 ~ s2
      val r =
        Graph.from(signals, edges)
      //println("isolation graph built")
      r
    }
    val result = markCliques(isolationGraph)
      .map(c => (c, domainSystemModel))
    //println(s"${signals.size} signals split into ${result.size} groups")
    result
  }

  override def transformToCalculationModel(problemPart: DomainProblemPart): CalculationModelInput =
    val (signals, model) = problemPart
    val (g1, source) = addSource(signals.toSeq, model.graph)
    val (g2, sink) = addSink(g1, model.workstations, model.workstationDisplays)
    PreparedGraph(
      signals,
      source,
      sink,
      g2,
      model.workstations,
      model.workstationDisplays
    )
    
    
  override def calculateModel(calculationInput: CalculationModelInput): CalculationModelOutput =
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
      val maxFlowGraph = jMaxFlow(g, source, sink)

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

  override def interpretModel(calculationResult: CalculationModelOutput): DomainSolutionPart =
    // Распаковка данных
    val ProcessedGraph(signals, completeFlow, originalWorkstations, workstationBunches) =
      calculationResult

    def cycledWorkstations(workstations: Seq[Int], i: Int = 0): LazyList[Int] =
      workstations(i) #:: cycledWorkstations(workstations, (i + 1) % workstations.length)

    val cycledOriginalWorkstations = cycledWorkstations(originalWorkstations)

    // рёбра отсортированные по шагам
    val actionsEdges = workstationBunches.map { bunch =>
      bunch.zip(cycledOriginalWorkstations).flatMap { (copy, original) =>
        (completeFlow get copy).edges
          .map(_.outer)
          .filter(e => e.target == copy && e.weight > 0)
          .map(e => (e, original))
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

  override def aggregateResults(domainResults: Iterable[DomainSolutionPart]
                               ): DomainSolutionWhole = domainResults.flatten

  type GT = scalax.collection.mutable.Graph[Int, UnDiEdge[Int]]
  // Разделение графа изоляции на клики
  def markCliques(graph: Graph[Int, UnDiEdge[Int]]): Seq[Set[Int]] =

    val g = scalax.collection.mutable.Graph.from[Int, UnDiEdge[Int]](
      graph.nodes.map(_.outer),
      graph.edges.map(_.outer)
    )
    val nodes = mutable.TreeSet.from(g.nodes)(Ordering[g.NodeT](_.degree - _.degree))
    val result = mutable.Buffer.empty[Set[Int]]

    while g.nodes nonEmpty do
      val clique = greedyClique(g.nodes.head, g)
      result.addOne(clique)
      g.removeAll(clique.map(g.get),Set.empty)

      //println(s"${clique.size} left: ${g.nodes.size}")
    Seq.from(result)

  override def name: String = "CliqueFlow"

  // Жадный алгоритм поиска клики включающей узел
  def greedyClique(node: GT#NodeT, graph: GT): Set[Int] = {
    val n = graph get node.outer
    // подграф из соседей
    val neighbourGraph = graph.filter(n.neighbors.union(Set(n)))
    // обязательная вершина
    val reqNode = neighbourGraph.get(n)

    def isClique(nodes: mutable.Set[neighbourGraph.NodeT]) =
      val filtered = neighbourGraph.filter(nodes)
      val size = filtered.nodes.size
      filtered.nodes.forall(_.degree == size-1)

    // Сортировка вершин по арности в подграфе соседей
    val ds = neighbourGraph.degreeNodeSeq
    var h = mutable.Set(reqNode)
    var max = 1

    val c = mutable.Set(reqNode)
    for {
      (d2, u) <- ds
      if isClique(c + u)
    } c.add(u)
    if (c.size > max) {
      h = c
      max = c.size
    }

    val builder = Set.newBuilder[neighbourGraph.NodeT]
    builder.sizeHint(max)
    builder.addAll(h).result().map(_.outer)
  }
}
