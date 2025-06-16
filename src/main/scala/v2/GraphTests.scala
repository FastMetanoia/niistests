package v2

import scalax.collection.edges.{UnDiEdge, UnDiEdgeImplicits}
import scalax.collection.immutable.Graph

import scala.collection.mutable

object GraphTests :
  @main def testFilter() =

    val g = Graph.from(Seq(5, 6, 7, 8),Seq(
      5~6, 6~7, 7~8
    ))
    val f = Set(5,6,7).map(g.get)
    println(g.filter(f))

  @main def testRemoval() =
    val g = Graph.from(Seq(5, 6, 7, 8), Seq(
      5 ~ 6, 6 ~ 7, 7 ~ 8
    ))
    val f = Set(5, 6, 7)
    println(g.removedAll(f,Set.empty))




  type GT = scalax.collection.mutable.Graph[Int, UnDiEdge[Int]]
  def isClique(graph: GT, nodes: mutable.Set[GT#NodeT]) =
    val filtered = graph.filter(nodes)
    println(filtered)
    val size = filtered.nodes.size
    filtered.nodes.forall(_.degree == size - 1)

  @main def testClique() =
    val g = scalax.collection.mutable.Graph.from(Seq(5, 6, 7, 8), Seq(
      5 ~ 6, 6 ~ 7, 7 ~ 8
    ))
    val f = Set(5, 6, 7 )
    println(isClique(g, mutable.Set.from(f.map(g.get))))