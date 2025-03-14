package v2

import java.util.concurrent.atomic.AtomicLong
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

object ParallelSolutionAuxiliaries:
  def duplicateWorkstations(
      g: Graph[Int, WDiEdge[Int]],
      ws: Seq[Int],
      wsMap: Map[Int, Int]
  ): (Graph[Int, WDiEdge[Int]], Seq[Int], Map[Int, Int]) =
    // oldId->(newId, displaysNumber)
    val oldNewMapping = wsMap.map { (oldId, displaysNumber) =>
      oldId -> (GlobalAuxiliaries.generateId() , displaysNumber)
    }

    val newEdges = oldNewMapping.flatMap { elem =>
      val (oldId, (newId, displaysNumber)) = elem
      (g get oldId).diPredecessors.map(_.outer).map { pred =>
        // todo: 0 or 1?
        pred ~> newId % 0
      }
    }

    val newMapping = oldNewMapping.map { elem =>
      val (oldId, (newId, displaysNumber)) = elem
      newId -> displaysNumber
    }

    val newWorkstations = newMapping.keySet

    val newGraph =
      Graph.from(
        g.nodes.toOuter ++ newWorkstations,
        g.edges.toOuter ++ newEdges
      )
    (newGraph, newWorkstations.toSeq, newMapping)
  end duplicateWorkstations

  case class PreparedGraph(
    signals: Set[Int],
    source: Int,
    sink: Int,
    graph: Graph[Int, WDiEdge[Int]],
    workstations: Seq[Int],
    workstationDisplays: Map[Int, Int]
  )

  case class ProcessedGraph(
      signals: Set[Int],
      graph: Graph[Int, WDiEdge[Int]],
      workstationBunches: Seq[Seq[Int]]
  )

  // sutturation check
  def isSutturated(
      flowGraph: Graph[Int, WDiEdge[Int]],
      signals: Seq[Int],
      sutturationMapping: Map[Int, Int]
  ): Boolean =
    sutturationMapping.forall { (signalId, successorsNumber) =>
      (flowGraph get signalId).edges
        .map(_.outer)
        .filter(_.source == signalId)
        .map(_.weight)
        .sum == successorsNumber
    }
  
  def FordFulkersonMaximumFlow(
    graph: Graph[Int, WDiEdge[Int]],
    source: Int,
    destination: Int
  ): Graph[Int, WDiEdge[Int]] =
    /*
    Алгоритм Форда-Фалкерсона для максимального потока.

    Создаём мутабельный граф из нашего немутабельного.
    Наш оригинальный граф - отражает пропускные способности.
    Вновь созданный - граф потока.

    В графе потока строятся фиктивные рёбра, направленные против реальных рёбер.
    Пропускная способность фиктивных рёбер C(vu) = 0

    Поток через прямое ребро и поток через соответствующее ему фиктивное ребро антисимметричен.
    Т.е. f(uv) = -f(vu)

    Инвариант.
    1. Поток через ребро не больше пропускной способности ребра. f(uv) <= C(uv), f(vu) <= C(vu)
    2. Антисимметрия с обратными ребрами. f(uv) = -f(vu)
    3. Закон сохранения потока: Сумма потока входящих рёбер = 0 (за счёт фиктивных рёбер.)

    Далее, собственно алгоритм.
    1. Найти в остаточной сети путь из истока в сток по положительным рёбрам.
    2. Найти на пути минимальное ребро (у нас всегда 1)
    3. Вычесть в остаточной сети из веса каждого ребра на найденном пути вес минимального.
    4. Добавить к пути на графе потока величину минимального ребра.
    */

    val falseEdges = graph.edges.toOuter.map {
      // фиктивные рёбра
      case s :~> d % w => d ~> s % 0
    }

    // Граф пропускных способностей.
    val throughputGraph = Graph.from(
      graph.nodes.toOuter,
      graph.edges.toOuter ++ falseEdges
    )

    // Начальный граф потока: поток через все рёбра = 0: f(uv) = 0
    var flowGraph = Graph.from(
      throughputGraph.nodes,
      throughputGraph.edges.toOuter.map { case s :~> d % w =>
        s ~> d % 0
      }
    )

    // Остаточная сеть
    // Вес ребра = C(uv) - f(uv), начальная в точности равна C(uv)
    val risidualNet = Graph.from(
      throughputGraph.nodes.toOuter,
      throughputGraph.edges.toOuter
    )

    // Пробуем увеличить поток:
    // Нам нужен путь от истока к стоку, для которого верно, что для всех пар вершин u и v: C(uv) - f(uv) > 0
    // То есть, путь в остаточной сети, где веса всех рёбер больше 0

    def risidualNetSequence(
        risidualNet: Graph[Int, WDiEdge[Int]]
    ): LazyList[Graph[Int, WDiEdge[Int]]] =
      // Выкинем из остаточной сети все рёбра <= 0
      val positiveRisidualNet =
        Graph from (risidualNet.edges.toOuter.filter(edge => edge.weight > 0))
      // Найдём путь
      val path =
        (positiveRisidualNet get source) pathTo (positiveRisidualNet get destination)

      path match
        // Если путь не найден - поток максимален.
        case None => LazyList.empty
        // Если путь найден
        case Some(path) =>
          // Наименьший вес на пути
          val smallestWeight = path.edges.minBy(e => e.weight).weight
          // Путь с прямыми и обратными рёбрами
          val adjustedPath = path.edges
            .flatMap(e => {
              val s :~> d % w = e.outer
              Seq(
                // Уменьшаем на прямом пути. Это означает увеличение потока
                s ~> d % (w - smallestWeight),
                // Увеличиваем на обратном(?). Это означает увеличение потениального обратного потока.
                d ~> s % (w + smallestWeight)
              )
            })
            .toSeq

          // Новая остаточная сеть. Заменяем пути из старой сети на новые.
          val updatedRisidualNet = Graph.from(
            positiveRisidualNet.nodes.toOuter,
            // Убираем все рёбра по пути
            positiveRisidualNet.edges.toOuter.filter(edge =>
              adjustedPath
                .find(e => e.source == edge.source && e.target == edge.target)
                .isEmpty
            )
            // Добавляем все рёбра по пути
              ++ adjustedPath
          )
          updatedRisidualNet #:: risidualNetSequence(updatedRisidualNet)

    // Итоговый вариант остаточной сети.
    val finalRisidualNet = risidualNetSequence(risidualNet).last

    // Способ доставать рёбра за амортизированную O(1)
    val edgeMap = finalRisidualNet.edges.toOuter
      .map(edge => (edge.source, edge.target) -> edge.weight)
      .toMap
      .withDefault((_, _) => 0.0)

    // Итоговый граф потока
    val finalFlowGraph = Graph.from(
      throughputGraph.nodes.toOuter,
      throughputGraph.edges.toOuter.map { case s :~> d % w =>
        s ~> d % (w - edgeMap((s, d)))
      }
    )
    finalFlowGraph
  end FordFulkersonMaximumFlow