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