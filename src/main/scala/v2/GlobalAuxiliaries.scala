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
import scalax.collection.AnyGraph
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import v2.GlobalAuxiliaries.*
import scala.concurrent.Promise
import scala.util.Success

object GlobalAuxiliaries:
  private val currentId:Promise[AtomicInteger] = Promise()

  
  def initializeIdGenerator(n:Int = 0):Unit = 
    currentId.complete(Success(AtomicInteger(n)))

  def generateId():Int = 
    Await.result(currentId.future, Duration.Inf).incrementAndGet()

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
    *   lower and higher edges for workstation display number. Random number,
    *   flat distribution.
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
    val signalNodes = (1 to signals).map(_ => GlobalAuxiliaries.generateId())
    val videoNodes = (1 to videoshots).map(_ => GlobalAuxiliaries.generateId())
    val workstationNodes =
      (1 to workstations).map(_ => GlobalAuxiliaries.generateId())

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
      w <- pick(shot2Workstations, workstationNodes.length).map(
        workstationNodes
      )
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

  def printCollection[X](title: String, coll: Iterable[X]): String =
    s"$title:\n\t${coll.mkString("\n\t")}\n"

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

  def addSink(
      graph: Graph[Int, WDiEdge[Int]],
      workstations: Seq[Int],
      workstationDisplays: Map[Int, Int]
  ): (Graph[Int, WDiEdge[Int]], Int) =
    val sink = GlobalAuxiliaries.generateId()
    val sinkEdges = workstations.map(w => w ~> sink % workstationDisplays(w))
    (
      Graph.from(graph.nodes.toOuter + sink, graph.edges.toOuter ++ sinkEdges),
      sink
    )
  def addSourceAndSink(
    graph:Graph[Int, WDiEdge[Int]], 
    signals:Seq[Int], 
    workstations:Seq[Int], 
    workstationDisplays:Map[Int, Int] ):(Int,Int,Graph[Int, WDiEdge[Int]]) = 
      val (g1, source) = addSource(signals, graph)
      val (g2, sink) = addSink(g1, workstations, workstationDisplays)
      (source, sink, g2)
  def removeSourceAndSink(
      graph: Graph[Int, WDiEdge[Int]],
      source: Int,
      sink: Int
  ): Graph[Int, WDiEdge[Int]] =
    graph - source - sink

  def addSource(
      signals: Seq[Int],
      graph: Graph[Int, WDiEdge[Int]]
  ): (Graph[Int, WDiEdge[Int]], Int) =
    given Graph[Int, WDiEdge[Int]] = graph
    val source = GlobalAuxiliaries.generateId()
    val sourceEdges =
      signals.map(s => source ~> s % (graph get s).diSuccessors.size)
    (
      Graph.from(
        graph.nodes.toOuter + source,
        graph.edges.toOuter ++ sourceEdges
      ),
      source
    )
  def writeGraphToGraphML(
      graph: Graph[Int, WDiEdge[Int]],
      fileName: String,
      signals: Set[Int],
      videoshots: Set[Int],
      workstations: Set[Int]
  ): Unit =
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

    val nodes = graph.nodes.toOuter.map { n =>
      val color = n match
        case x: Int if (signals.contains(x))      => "red"
        case x: Int if (videoshots.contains(x))   => "green"
        case x: Int if (workstations.contains(x)) => "blue"
        case _                                    => "black"
      s"\t\t<node id=\"n$n\">\n" + "\t\t\t<data key=\"d0\">" + color + "</data>\n" + "\t\t</node>"
    }

    val edges = graph.edges.toOuter.zipWithIndex.map { (e, i) =>
      s"\t\t<edge id=\"e$i\" source=\"n${e.source}\" target=\"n${e.target}\">\n" +
        "\t\t\t<data key=\"d1\">" + e.weight.toDouble + "</data>\n" + "\t\t</edge>"
    }
    val epilog = """
  </graph>
</graphml>"""
    val composed =
      prolog + nodes.mkString("\n") + "\n" + edges.mkString("\n") + epilog

    val file = os.temp.dir() / (fileName + ".graphml")
    os.write.over(file, composed)
    println(s"graph saved to ${file}")

  def showNodesSuccessors(
      setName: String,
      graph: Graph[Int, WDiEdge[Int]],
      nodeSet: Seq[Int]
  ): String =
    printCollection(
      setName,
      nodeSet.map { s =>
        s"$s-> " + (graph get s).diSuccessors
          .map(_.outer)
          .mkString("(", ", ", ")")
      }
    )
