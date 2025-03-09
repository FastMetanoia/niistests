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
import com.example.Trivials.FunctorFunctor.map
import com.example.Trivials.StringFilter.filter
import scalax.collection.edges.UnDiEdge
import scalax.collection.AnyGraph

/**
  * Класс для представления тестируемой системы.
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
    given Graph[Int, WDiEdge[Int]] = graph
    def printNodesSuccessors(setName:String, nodeSet:Seq[Int]):String = 
      printCollection(setName, 
        nodeSet.map{s=>
          s"$s-> " + n(s).diSuccessors.map(_.outer).mkString("(",", ",")")
          }
      )
    val signalsPrint = signals
    printNodesSuccessors("Signals", signals) + 
    printNodesSuccessors("Videoshots", videoshots) +
    printNodesSuccessors("Workstations", workstations) + 
    printCollection("Workstation Displays", workstationDisplays)
def printCollection[X](title:String, coll:Iterable[X]):String = 
  s"$title:\n\t${coll.mkString("\n\t")}\n"
object SystemModel


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
    videoshots: Int,
    workstations: Int,
    signal2Shots: Int,
    shot2Workstations: Int,
    displayLimits: (Int, Int)
): SystemModel =

  // Nodes
  val signalNodes = (1 to signals).map(_ => IdGen())
  val videoNodes = (1 to videoshots).map(_ => IdGen())
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
    w <- pick(shot2Workstations, workstationNodes.length).map(workstationNodes)
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
    // собираем итератор.
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

  // Способ доставать рёбра за амортизированную O(1)
  val edgeMap = finalRisidualNet.edges.toOuter
    .map(edge=>(edge.source, edge.target)->edge.weight).toMap
    .withDefault((_,_)=>0.0)

  // Итоговый граф потока
  val finalFlowGraph = Graph.from(
    throughputGraph.nodes.toOuter,
    throughputGraph.edges.toOuter.map{
      case s:~>d % w => s~>d % (w - edgeMap((s,d)))
    }
  )
  finalFlowGraph
end FordFulkersonMaximumFlow
def addSink(
  graph:Graph[Int, WDiEdge[Int]], 
  workstations:Seq[Int], 
  workstationDisplays:Map[Int,Int]
  ):(Graph[Int, WDiEdge[Int]], Int) =
    val sink = IdGen()
    val sinkEdges = workstations.map(w=> w ~> sink % workstationDisplays(w))
    (Graph.from(graph.nodes.toOuter + sink, graph.edges.toOuter ++ sinkEdges), sink)
  
def removeSourceAndSink(graph:Graph[Int, WDiEdge[Int]], source:Int, sink:Int):Graph[Int, WDiEdge[Int]] =
  graph - source - sink

def addSource(
  signals:Seq[Int], 
  graph:Graph[Int, WDiEdge[Int]]
  ):(Graph[Int, WDiEdge[Int]], Int) = 
    given Graph[Int, WDiEdge[Int]] = graph
    val source = IdGen()
    val sourceEdges = signals.map(s => source ~> s % n(s).diSuccessors.size)
    (Graph.from(graph.nodes.toOuter + source, graph.edges.toOuter ++ sourceEdges), source)

// fallback for case when unable to process all signal-videoshot combinations with 1 action
// returns the result graph, newly created workstations and mapping
def duplicateWorkstations(
  g:Graph[Int, WDiEdge[Int]], 
  ws:Seq[Int], 
  wsMap:Map[Int, Int]
  ):(Graph[Int, WDiEdge[Int]], Seq[Int], Map[Int,Int]) = 
  // oldId->newId->displaysNumber
  val oldNewMapping = wsMap.map{ (k,v)=>k->(IdGen()->v) }

  val newEdges = oldNewMapping.flatMap{ elem=>
    val (oldId, (newId, displaysNumber)) = elem
    (g get oldId).diPredecessors.map(_.outer).map{ pred=> 
        // todo: 0 or 1? 
        pred ~> newId % 0
      }
    }
  
  val newMapping = oldNewMapping.map{ elem=>
    val (oldId, (newId, displaysNumber)) = elem
    newId->displaysNumber
  }

  val newWorkstations = newMapping.keySet
  
  val newGraph =
    Graph.from(
      g.nodes.toOuter ++ newWorkstations,
      g.edges.toOuter ++ newEdges
    )
  (newGraph, newWorkstations.toSeq, newMapping)
end duplicateWorkstations

    /**
      * @param signals              signals to process simulataniously
      * @param graph                graph to work with
      * @param workstations         workstation nodes
      * @param workstationDisplays  workstation to display number mapping
      * @return                     LazyList of actions
      */
def processSignals(
  signals:Seq[Int], 
  graph:Graph[Int, WDiEdge[Int]], 
  workstations:Seq[Int], 
  workstationDisplays:Map[Int, Int]):Seq[Action] = 

    // number of connections for each signal
    val signalsSutturation = signals.map(s=> s->(graph get s).diSuccessors.size).toMap

    // sutturationCheck
    def isSutturated(
      flowGraph:Graph[Int, WDiEdge[Int]],
      signals:Seq[Int], 
      sutturationMapping:Map[Int,Int]
    ):Boolean =
      sutturationMapping.forall{ (signalId, successorsNumber)=>
        (flowGraph get signalId).edges
          .map(_.outer)
          .filter(_.source == signalId)
          .map(_.weight)
          .sum == successorsNumber
      }
    
    def go( 
      sigs:Seq[Int], 
      g:Graph[Int, WDiEdge[Int]], 
      ws:Seq[Seq[Int]], 
      wsDisplays:Seq[Map[Int, Int]]
    ):LazyList[(Graph[Int, WDiEdge[Int]], Seq[Seq[Int]])] = 
      val (g1, source) = addSource(sigs, g)
      val (g2, sink) = addSink(g1, ws.flatten, wsDisplays.flatten.toMap)
      val maxFlowGraph = removeSourceAndSink(FordFulkersonMaximumFlow(g2, source, sink), source, sink)
      // All edges are sutturated than we have a final flow version
      if(isSutturated(maxFlowGraph, sigs, signalsSutturation))
        LazyList((maxFlowGraph, ws))
      else 
        // if not, we duplicate the workstations and recalculate the flow
        val (newG, newWs, newWsMapping) = duplicateWorkstations(g, workstations, workstationDisplays)
        (maxFlowGraph, ws) #:: go(
          sigs,
          newG,
          ws.appended(newWs),
          wsDisplays.appended(newWsMapping)
        )

    //   полный поток и коответствующее количество копий рабочих станций
    val (completeFlow, workstationBunches) = go(signals, graph, Seq(workstations), Seq(workstationDisplays)).last
    
    // рёбра отсортированные в по шагам
    val actionsEdges = workstationBunches.map{ _.flatMap{ worstation=>
      (completeFlow get worstation)
        .edges.map(_.outer)
        .filter(e=> e.target == worstation && e.weight > 0)
      }}

    // итог: действия
    val actions = actionsEdges.map{ edges => 
      Action(
        signals.toSet,
        edges.map(e=> e.source->e.target).toMap
      )
    }
    actions
end processSignals

// def writeGraphToDotFile[N,E](graph:Graph[N,E], fileName:String):Unit = 
//   import scalax.collection.io.dot.*
//   ???

def writeGraphToGraphML(
  graph:Graph[Int, WDiEdge[Int]], 
  fileName:String, 
  signals:Set[Int], 
  videoshots:Set[Int], 
  workstations:Set[Int]):Unit = 
    val prolog = """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns"  
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
    http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
  <graph id="G" edgedefault="directed">
  <key id="d0" for="node" attr.name="color" attr.type="string">
  <default>yellow</default>
  </key>
  <key id="d1" for="edge" attr.name="weight" attr.type="double"/>
    """

    val nodes = graph.nodes.toOuter.map{n=>
      val color = n match
        case x:Int if(signals.contains(x)) => "red"
        case x:Int if(videoshots.contains(x)) => "green"
        case x:Int if(workstations.contains(x)) => "blue"
        case _ => "black"
      s"\t\t<node id=\"n$n\">\n" +"\t\t\t<data key=\"d0\">" + color + "</data>\n"+ "\t\t</node>"
    }

    val edges = graph.edges.toOuter.zipWithIndex.map{(e, i)=>
      s"\t\t<edge id=\"e$i\" source=\"n${e.source}\" target=\"n${e.target}\">\n" + 
      "\t\t\t<data key=\"d1\">" + e.weight.toDouble + "</data>\n" + "\t\t</edge>"
    }
    val epilog = """
  </graph>
</graphml>"""
    val composed = prolog + nodes.mkString("\n") +"\n"+ edges.mkString("\n") + epilog
    
    val file = os.temp.dir() / (fileName + ".graphml")
    os.write.over(file, composed)
    println(s"graph saved to ${file}")

  
// todo :
def isolationGraph(signals:Seq[Int], systemGraph:Graph[Int, WDiEdge[Int]]):Graph[Int, UnDiEdge[Int]] = 
  for{
    s1 <- signals
    s2 <- signals
    if s1 != s2
    es1 = (systemGraph get s1).edges.map(_.outer).filter(_.source == s1)
    es2 = (systemGraph get s2).edges.map(_.outer).filter(_.source == s2)
  } ???
  ???

def fulcersonTest() = 
  val systemModel:SystemModel = generateSystemModel(
    signals = 10000, 
    videoshots = 1000, 
    workstations = 20, 
    signal2Shots = 10, 
    shot2Workstations = 5, 
    displayLimits = (3,5)
    )
  writeGraphToGraphML(
    systemModel.graph, 
    "a",
    systemModel.signals.toSet, 
    systemModel.videoshots.toSet, 
    systemModel.workstations.toSet)
  


object SystemGraphOps{
  
}

@main def mmain() = 
  //fulcersonTest()
  
  val file = os.read(os.Path("""C:\Users\Diemy\AppData\Local\Temp\7862956825990604953\a.graphml"""))
  println(file.split("\n").length)

