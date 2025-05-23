


import org.jgrapht
import org.jgrapht.*
import org.jgrapht.graph.*

import java.net.URI
import scala.jdk.CollectionConverters.*

// jGraphT example
@main def jGraphTest():Unit =
  val g = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])
  val google = new URI("http://www.google.com")
  val wikipedia = new URI("http://www.wikipedia.org")
  val jgrapht = new URI("http://www.jgrapht.org")

  // add the vertices
  g.addVertex(google)
  g.addVertex(google)
  g.addVertex(wikipedia)
  g.addVertex(jgrapht)

  // add edges to create linking structure
  g.addEdge(jgrapht, wikipedia)
  g.addEdge(google, jgrapht)
  g.addEdge(google, wikipedia)
  g.addEdge(wikipedia, google)

  val start = g.vertexSet().asScala
    .find(uri => uri.getHost.equals("www.jgrapht.org")).get
  println(start)
  println(g)

@main def zipTest():Unit =
  def cycledWorkstations(workstations: Seq[Int], i: Int = 0): LazyList[Int] =
    workstations(i) #:: cycledWorkstations(workstations, (i + 1) % workstations.length)

  println(s"cycledWorkstations(Seq(1,2,3)).take(10).toVector = ${cycledWorkstations(Seq(1,2,3)).take(10).toVector}")





//sealed trait Value
//case class Is(x:Int) extends Value
//case class Js(x:Int) extends Value
//
//case class MyEdge(from:Value, to:Value) extends AbstractDiEdge[Value](from, to)
//
//@main def gTests():Unit =
//    val g:Graph[Value, MyEdge] =
//        Graph.from(
//            Seq(Js(1), Is(2)),
//            Seq(MyEdge(Js(1), Is(2)))
//        )
//
//    println(g)
//
//    val fn = g.nodes.find{ _.outer.isInstanceOf[Is]}
//    println(fn)
//    val cn = g.nodes.collectFirst{
//        case n:g.NodeT if n.outer.isInstanceOf[Is] => n.outer
//    }
//    println(cn)
//
//    println(Seq(1,2,3,4,5).sortBy(x=>x))
//
//@main def testMain():Unit =
//  println((1 to 3).map(x=>x))
//  val g = Graph.from[Int, WDiEdge[Int]](Seq(1,2,3), Seq(1~>2 % 1, 2~>3 % 2, 3~>1 % 2))
//  // modification
//  val ne = g.map(
//    fNode = _.outer,
//    fEdge = (edge, _, _) =>
//      val s:~>d % w = edge.outer
//      s ~> d % (w + 1)
//  )
//
//  // println(g.addOne(1~>3 % 3))
//  println((g get 1).edges.filter(e=>e.outer.source == 1).map(e=>e.outer.weight).sum)