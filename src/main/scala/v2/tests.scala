package v2

import scalax.collection.mutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import scalax.collection.edges.labeled.:~>
import scalax.collection.edges.labeled.%

@main def testMain():Unit =
  val g = Graph.from[Int, WDiEdge[Int]](Seq(1,2,3), Seq(1~>2 % 1, 2~>3 % 2, 3~>1 % 2))
  // modification
  val ne = g.map(
    fNode = _.outer,
    fEdge = (edge, _, _) => 
      val s:~>d % w = edge.outer
      s ~> d % (w + 1)
  )

  // println(g.addOne(1~>3 % 3))
  println((g get 1).edges.filter(e=>e.outer.source == 1).map(e=>e.outer.weight).sum)