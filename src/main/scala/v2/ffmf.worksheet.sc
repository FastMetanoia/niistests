import v2.GlobalAuxiliaries.initializeIdGeneratorIfNot
initializeIdGeneratorIfNot()

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
import v2.SystemModel
import v2.DataToTestOn



val SystemModel(g, signals, videoshots, workstations, workstationDisplays) = DataToTestOn.Simplest.model
val (destination, source, graph) = addSourceAndSink(g, signals, workstations, workstationDisplays)


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
val residualNet = Graph.from(
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
 val positiveResidualNet =
   Graph from (risidualNet.nodes.toOuter, risidualNet.edges.toOuter.filter(edge => edge.weight > 0))
 // Найдём путь
 val path =
   (positiveResidualNet get source) pathTo (positiveResidualNet get destination)

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
     val updatedResidualNet = Graph.from(
       positiveResidualNet.nodes.toOuter,
       // Убираем все рёбра по пути
       positiveResidualNet.edges.toOuter.filterNot(edge =>
         adjustedPath.exists(e => e.source == edge.source && e.target == edge.target)
       )
       // Добавляем все рёбра по пути
         ++ adjustedPath
     )
     updatedResidualNet #:: risidualNetSequence(updatedResidualNet)

// Итоговый вариант остаточной сети.
val finalResidualNet = risidualNetSequence(residualNet).last

// Способ доставать рёбра за амортизированную O(1)
val edgeMap = finalResidualNet.edges.toOuter
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