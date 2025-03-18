

import scalax.collection.mutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import scalax.collection.edges.labeled.:~>
import scalax.collection.edges.labeled.%

import scalax.collection.mutable.Graph
import scalax.collection.edges.DiEdge
import scalax.collection.edges.DiEdgeImplicits 
import scalax.collection.generic.AnyEdge
import scalax.collection.generic.AbstractDiEdge

sealed trait Value
case class Is(x:Int) extends Value
case class Js(x:Int) extends Value

case class MyEdge(from:Value, to:Value) extends AbstractDiEdge[Value](from, to)

@main def gTests():Unit = 
    val g:Graph[Value, MyEdge] = 
        Graph.from(
            Seq(Js(1), Is(2)),
            Seq(MyEdge(Js(1), Is(2)))
        )
    
    println(g)

    val fn = g.nodes.find{ _.outer.isInstanceOf[Is]}
    println(fn)
    val cn = g.nodes.collectFirst{
        case n:g.NodeT if n.outer.isInstanceOf[Is] => n.outer
    }
    println(cn)

    println(Seq(1,2,3,4,5).sortBy(x=>x))

@main def testMain():Unit =
  println((1 to 3).map(x=>x))
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