// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import v2.ParallelSolutionAuxiliaries
import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import v2.GlobalAuxiliaries.showNodesSuccessors
import v2.SystemModel



class Saturation extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  test("saturation positive check"){
    val saturationMapping = Map(1->3)

    val signals = Seq(1)
    val videoshots = Seq.empty[Int]
    val workstations = Seq.empty[Int]
    val workstationDisplays = Map.empty[Int, Int]

    val graph = Graph.from[Int, WDiEdge[Int]](
      signals ++ Seq(2,3,4),
      Seq(1~>2 % 1, 1~>3 % 1, 1~>4 % 1)
    )

    val result = ParallelSolutionAuxiliaries.isSaturated(graph, signals, saturationMapping)
    assert(result, s"must be true for graph: \n$graph \nReturned $result")
  }

  test("sutturation negative check".fail){
    val sutturationMapping = Map(1->3)

    val signals = Seq(1)
    val videoshots = Seq.empty[Int]
    val workstations = Seq.empty[Int]
    val workstationDisplays = Map.empty[Int, Int]

    val graph = Graph.from[Int, WDiEdge[Int]](
      signals ++ Seq(2,3,4),
      Seq(1~>2 % 1, 1~>3 % 1, 1~>4 % 0)
    )

    val result = ParallelSolutionAuxiliaries.isSaturated(graph, signals, sutturationMapping)
    assert(result, s"must be false for graph: \n$graph \nReturned $result")
  }
}
