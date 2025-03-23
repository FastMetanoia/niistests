package v2

import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import v2.SystemModel
import v2.GlobalAuxiliaries.generateSystemModel
import v2.GlobalAuxiliaries.generateId

object DataToTestOn {
  object Minimal{
    val model = generateSystemModel(
      1,
      1,
      1,
      1,
      1,
      (1, 2)
    )
  }

  object Simple:
    val signals = Seq(generateId())
    val videoshots = (0 to 2).map(_=>generateId())
    val workstations = (0 to 3).map(_=>generateId())
    val workstationDisplays = Map(
      workstations(0) -> 1,
      workstations(1) -> 1,
      workstations(2) -> 1,
      workstations(3) -> 1
    )

    val edges1 = Seq(
      signals(0) -> videoshots(0),
      signals(0) -> videoshots(1),
      signals(0) -> videoshots(2),
      videoshots(0) -> workstations(0),
      videoshots(0) -> workstations(1),
      videoshots(1) -> workstations(1),
      videoshots(1) -> workstations(2),
      videoshots(2) -> workstations(2),
      videoshots(2) -> workstations(3)
    )

    val edges2 = Seq(
      signals(0) -> videoshots(0),
      signals(0) -> videoshots(1),
      signals(0) -> videoshots(2),
      videoshots(0) -> workstations(0),
      videoshots(0) -> workstations(1),
      videoshots(1) -> workstations(0),
      videoshots(2) -> workstations(2),
      videoshots(2) -> workstations(3)
    )

    val g1 = Graph.from[Int, WDiEdge[Int]](
      nodes = signals ++ videoshots ++ workstations,
      edges = edges1.map { (s, t) => s ~> t % 1 }
    )
    
    val g2 = Graph.from[Int, WDiEdge[Int]](
      nodes = signals ++ videoshots ++ workstations,
      edges = edges2.map { (s, t) => s ~> t % 1 }
    )

    val m1 =
      SystemModel(g1, signals, videoshots, workstations, workstationDisplays)
    val m2 =
      SystemModel(g2, signals, videoshots, workstations, workstationDisplays)

    val notFullModel = {
      val signals = Seq(generateId())
      val videoshots = (0 to 3).map(_=>generateId())
      val workstations = (0 to 1).map(_=>generateId())

      val workstationDisplays = Map(
        workstations(0) -> 1,
        workstations(1) -> 1
      )

      val edges =
        videoshots.map(vs=> signals(0) -> vs) ++ Seq(
          videoshots(0)->workstations(0),
          videoshots(1)->workstations(1),
          videoshots(2)->workstations(1),
          videoshots(3)->workstations(1),
        )
      val graph = Graph.from(
        nodes = signals ++ videoshots ++ workstations,
        edges = edges.map { (s, t) => s ~> t % 1 }
      )

      SystemModel(
        graph,
        signals,
        videoshots,
        workstations,
        workstationDisplays
      )
    }
}

