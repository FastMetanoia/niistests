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
import scala.jdk.CollectionConverters.*

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

  // saturation check
  def isSaturated(
                   flowGraph: Graph[Int, WDiEdge[Int]],
                   signals: Seq[Int],
                   saturationMapping: Map[Int, Int]
  ): Boolean =
    signals.forall { signalId =>
      val saturatedEdges = (flowGraph get signalId).edges
        .map(_.outer)
        .filter(_.source == signalId)
        .filter(_.weight == 1d)
      val number = saturatedEdges
        .size
      number == saturationMapping(signalId)
    }

  def jMaxFlow(
               graph: Graph[Int, WDiEdge[Int]],
               source: Int,
               sink: Int
             ): Graph[Int, WDiEdge[Int]] =
    import org.jgrapht.graph.SimpleDirectedWeightedGraph
    import org.jgrapht.graph.DefaultWeightedEdge

    import org.jgrapht.alg.flow.BoykovKolmogorovMFImpl as BKMF

    val g = SimpleDirectedWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    graph.nodes.foreach(n=> g.addVertex(n.outer))
    graph.edges.toOuter.foreach(e=>
      val edge = g.addEdge(e.source, e.target)
      g.setEdgeWeight(edge, e.weight)
    )

    val bkmf = BKMF(g)
    val result = bkmf.getMaximumFlow(source, sink)
    val flowMap = result.getFlowMap.asScala
    val edges = flowMap.map {
      (edge, flow) => g.getEdgeSource(edge) ~> g.getEdgeTarget(edge) % flow
    }

    Graph.from(
      graph.nodes.toOuter,
      edges
    )

  end jMaxFlow