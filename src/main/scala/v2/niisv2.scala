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

/** @param graph
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
)
object SystemModel
// def unapply(sm:SystemModel):Option[
//   (Graph[Int, WDiEdge[Int]],Seq[Int],Seq[Int],Seq[Int],Map[Int, Int])] =
//     Some((sm._1,sm._2,sm._3,sm._4,sm._5))

// Id ~ Int, последовательный
object IdGen:
  private val nextId = AtomicInteger(0)
  def apply() = nextId.incrementAndGet()

/** @param signals
  *   number of signals
  * @param videoshots
  *   number of videoshots
  * @param workstations
  *   number of workstations
  * @param signal2Shots
  *   number of shots connected with each signal
  * @param shots2WorkStations
  *   number of workstations connected to each shot
  * @param displayLimits
  *   lower and higher edges for workstation display number. Random number, flat
  *   distribution.
  */

def generateSystemModel(
    signals: Int,
    videoShots: Int,
    workstations: Int,
    signal2Shots: Int,
    shots2WorkStations: Int,
    displayLimits: (Int, Int)
): SystemModel =

  // Nodes
  val signalNodes = (1 to signals).map(_ => IdGen())
  val videoNodes = (1 to videoShots).map(_ => IdGen())
  val workstationNodes = (1 to workstations).map(_ => IdGen())

  // Случайные количества дисплеев.
  val workstationDisplays = workstationNodes
    .map(x => x -> Random.between(displayLimits._1, displayLimits._2))
    .toMap

  // Edges
  // Создаваемые рёбра имеют вес 1. Это потребуется позже для алгоритма поиска максимального потока.
  val svEdges = for {
    s <- signalNodes
    v <- pick(signal2Shots, videoNodes.length).map(videoNodes)
  } yield s ~> v % 1

  val vwEdges = for {
    v <- videoNodes
    w <- pick(shots2WorkStations, workstationNodes.length).map(workstationNodes)
  } yield v ~> w % 1

  // Собирвем граф
  val graph = Graph.from(
    signalNodes ++ videoNodes ++ workstationNodes,
    svEdges ++ vwEdges
  )

  SystemModel(
    graph,
    signalNodes,
    videoNodes,
    workstationNodes,
    workstationDisplays
  )

/** @param n
  *   количество выбираемых чисел
  * @param rangeLimit
  *   верхняя строгая граница
  * @return
  *   n различных целых чисел из интервала от 0 до rangeLimit - 1
  */
def pick(n: Int, rangeLimit: Int) =
  if (n > rangeLimit)
    throw IllegalArgumentException(
      s"An attempt to pick $n unique indices from 0 to $rangeLimit range"
    )
  def pickDifferent(chosen: Seq[Int], rangeLimit: Int): Int =
    val candidate = Random.nextInt(rangeLimit)
    if (!chosen.contains(candidate)) candidate
    else pickDifferent(chosen, rangeLimit)

  (1 to n).foldLeft(Seq.empty[Int])((chosen, _) =>
    chosen.appended(pickDifferent(chosen, rangeLimit))
  )

case class Action(signals: Set[Int], videoWorkstationMapping: Map[Int, Int])

trait TestingAlgorithmProducer:
  def actionIterator(system: SystemModel): Iterator[Action]
  def actionStream(system: SystemModel): LazyList[Action] =
    LazyList.from(actionIterator(system))

def n(using graph: Graph[Int, WDiEdge[Int]])(outer: Int): graph.NodeT =
  graph get outer

/** Последовательный алгоритм. Один шаг = один сигнал на одном видеокадре
  */
object Sequential extends TestingAlgorithmProducer:
  override def actionIterator(system: SystemModel): Iterator[Action] =
    // распаковка модели
    val SystemModel(graph, signals, videos, workstations, workstationDisplays) =
      system
    // будем передавать граф неявно
    given Graph[Int, WDiEdge[Int]] = graph
    // собираем итератор. Если код падает здесь, у нас с кикими-то видеокадрами не связаны АРМы.
    signals.toIterator
      .flatMap(s => n(s).diSuccessors.map(v => s -> v))
      .map { (s, v) => (s, v, n(v).diSuccessors.head) }
      .map { (s, v, w) => Action(Set(s), Map(v.outer -> w.outer)) }

// Копирование одной или нескольких вершин со всеми инцидентными рёбрами.
def copyed(graph: Graph[Int, WDiEdge[Int]])(
    node: Int
): (Graph[Int, WDiEdge[Int]], Int) =
  given Graph[Int, WDiEdge[Int]] = graph
  val successors = n(node).diSuccessors.map(_.outer)
  val predecessors = n(node).diPredecessors.map(_.outer)
  val newNode = IdGen()
  val edges = successors.map(s => newNode ~> s % 1) ++ predecessors.map(p =>
    p ~> newNode % 1
  )
  (graph + newNode ++ edges, newNode)

def copyed(graph: Graph[Int, WDiEdge[Int]])(
    nodes: Seq[Int]
): (Graph[Int, WDiEdge[Int]], Seq[Int]) =
  nodes.foldLeft((graph, Seq.empty[Int])) { case ((graph, sequence), oldNode) =>
    val (copyedGraph, newNode) = copyed(graph)(oldNode)
    (copyedGraph, sequence.appended(newNode))
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

  // def e(graph:Graph[Int, WDiEdge[Int]])(s:Int, d:Int):WDiEdge[Int] =
  //   graph get s ~> d %

  // Остаточная сеть
  // Вес ребра = C(uv) - f(uv), начальная в точности равна C(uv)
  val risidualNet = Graph.from(
    throughputGraph.nodes.toOuter,
    throughputGraph.edges.toOuter
  )

  // Пробуем увеличить поток:
  // Нам нужен путь от истока к стоку, для которого верно, что для всех пар вершин u и v: C(uv) - f(uv) > 0
  // То есть, путь в остаточной сети, где веса всех рёбер больше 0
  type GraphT = Graph[Int, WDiEdge[Int]]
  def risidualNetSequence(risidualNet: GraphT): LazyList[GraphT] =
    // Выкинем из остаточной сети все рёбра <= 0
    val positiveRisidualNet =
      Graph from (risidualNet.edges.toOuter.filter(edge => edge.weight > 0))
    // Найдём путь
    val path =
      (positiveRisidualNet get source) pathTo (positiveRisidualNet get destination)

    path match
      // Если путь не найден - поток максимален.
      case None       => LazyList.empty
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
          //Убираем все рёбра по пути
          positiveRisidualNet.edges.toOuter.filter(edge =>
            adjustedPath
              .find(e =>
                e.source == edge.source && e.target == edge.target
              )
              .isEmpty
          )
            // Добавляем все рёбра по пути
            ++ adjustedPath
        )
        updatedRisidualNet #:: risidualNetSequence(updatedRisidualNet)
  
  // Итоговый вариант остаточной сети.
  val finalRisidualNet = risidualNetSequence(risidualNet).last
  ???
