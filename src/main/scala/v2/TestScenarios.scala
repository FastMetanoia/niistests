package v2
import v2.GlobalAuxiliaries.*
import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory

def massTest() =
  val systemModel: SystemModel = GlobalAuxiliaries.generateSystemModel(
    signals = 10000,
    videoshots = 1000,
    workstations = 20,
    signal2Shots = 10,
    shot2Workstations = 5,
    displayLimits = (3, 5)
  )
  writeGraphToGraphML(
    systemModel.graph,
    "a",
    systemModel.signals.toSet,
    systemModel.videoshots.toSet,
    systemModel.workstations.toSet
  )


@main def mmain() =
  // fulcersonTest()

  val file = os.read(
    os.Path(
      """C:\Users\Diemy\AppData\Local\Temp\7862956825990604953\a.graphml"""
    )
  )
  println(file.split("\n").length)



@main def fulcersonTest() =

  val signals = Seq(1)
  val videoshots = Seq(2, 3, 4)
  val workstations = Seq(5, 6, 7, 8)
  val workstationDisplays = Map(
    5 -> 1,
    6 -> 1,
    7 -> 1,
    8 -> 1
  )

  val edges1 = Seq(
    1 -> 2,
    1 -> 3,
    1 -> 4,
    2 -> 5,
    2 -> 6,
    3 -> 6,
    3 -> 7,
    4 -> 7,
    4 -> 8
  )

  val edges2 = Seq(
    1 -> 2,
    1 -> 3,
    1 -> 4,
    2 -> 5,
    2 -> 6,
    3 -> 5,
    4 -> 7,
    4 -> 8
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

  import scala.util.chaining.scalaUtilChainingOps
  val scenrio1 = SimpleParallelTestingSolution.solveProblem(m1).tap(println)
  val scenrio2 = SimpleParallelTestingSolution.solveProblem(m2).tap(println)

@main def testSps() =
  massTest()
