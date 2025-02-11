import scalax.collection.*
import java.util.UUID
import scalax.collection.immutable.Graph
import scalax.collection.edges.DiEdge
import scala.util.Random
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.DiEdge
import scalax.collection.generic.Edge

sealed trait TestGraphNode
case object Source extends TestGraphNode
case object Sink extends TestGraphNode
case class Video(id: UUID) extends TestGraphNode
case class Sig(id: UUID) extends TestGraphNode
case class Workstation(id: UUID, displays: Int) extends TestGraphNode

trait TestingAction
case class ActualAction(
    signal: Sig,
    workstationsMapping: Map[Workstation, Set[Video]]
) extends TestingAction
case object AllDone extends TestingAction

case class SystemToTest(graph: Graph[TestGraphNode, DiEdge[TestGraphNode]])

trait TestingAlgorithm:
  def chooseAction(system: TestedSystem0): TestingAction
  def testingSequence(system: TestedSystem0): LazyList[TestingAction] =
    val action = chooseAction(system)
    system.applyAction(action)
    if (action == AllDone)
      LazyList(action)
    else
      action #:: testingSequence(system)

class TestedSystem0(val original: Graph[TestGraphNode, DiEdge[TestGraphNode]]):
  var state: mutable.Graph[TestGraphNode, DiEdge[TestGraphNode]] =
    mutable.Graph.from(
      original.nodes.map(_.outer).asInstanceOf[Iterable[TestGraphNode]],
      original.edges.map(_.outer).asInstanceOf[Iterable[DiEdge[TestGraphNode]]]
    )
  private var timer: Int = 0;
  def isTested: Boolean =
    state.edges.forall(edge => edge.node2.isInstanceOf[Workstation])
  def applyAction(action: TestingAction): Unit =
    action match
      case action: ActualAction =>
        val testedEdges: Iterable[DiEdge[TestGraphNode]] = for {
          (w, vs) <- action.workstationsMapping
          v <- vs
        } yield action.signal ~> v
        state.removeAll(
          testedEdges
        )
        timer += 1
      case AllDone =>

object SequencialAlgorithm0 extends TestingAlgorithm:
  override def chooseAction(system: TestedSystem0): TestingAction =
    // val nextPair = system.curState.edges.collectFirst{edge=>
    //     (edge.node1, edge.node2) match
    //         case (s:Sig, v:Video) =>
    //             val workstation = system.original get v findSuccessor (_.isInstanceOf[Workstation])
    //             workstation match
    //                 case Some(workstation:Workstation) => ActualAction(s, Map(workstation->Set(v)))
    // }
    // val f = system.state.nodes.find(_.outer.isInstanceOf[TestGraphNode])
    // println(f)
    val next = system.state.nodes.collectFirst {
      case s if s.outer.isInstanceOf[Sig] && s.diSuccessors.nonEmpty =>
        val v = s.diSuccessors.head
        val w = v.diSuccessors.head
        // val sig: Value = s.outer
        (s.outer, v.outer, w.outer)
    }

    // println(next)
    next match
      case Some(value) =>
        value match
          case (s: Sig, v: Video, w: Workstation) =>
            ActualAction(s, Map(w -> Set(v)))
      case None => AllDone

// Максимальный поток. Алгоритм Форда-Фалкерсона
def maximumBipartiteMatching(
    graph: Graph[TestGraphNode, DiEdge[TestGraphNode]]
): Set[DiEdge[TestGraphNode]] =
  type G = mutable.Graph[TestGraphNode, DiEdge[TestGraphNode]]
  val source = graph get Source
  val sink = graph get Sink
  val graphCopy =
    mutable.Graph.from(graph.nodes.outerIterable, graph.edges.outerIterable)
  def countPath(graph: G): Boolean =
    ???

  val videoNodes = graph.nodes.outerIterable.collect { case v: Video =>
    graph get v
  }.toIndexedSeq
  val workstationNodes = graph.nodes.outerIterable.collect {
    case w: Workstation => graph get w
  }.toIndexedSeq

  ???
end maximumBipartiteMatching

object SimpleGreedyAlgorithm0 extends TestingAlgorithm {

  override def chooseAction(system: TestedSystem0): TestingAction =
    val next = system.state.nodes.collectFirst {
      case s if s.outer.isInstanceOf[Sig] && s.diSuccessors.nonEmpty =>
        val vs = s.diSuccessors
        val ws = vs.map(_.diSuccessors)

        val vsIndex = vs.toSeq.sortBy(_.diSuccessors.size)
        // val wsMapping = ws.map{ workstationNode=>
        //     vsIndex.
        // }
        println(vs)
        // val sig: Value = s.outer
        // (s.outer,v.outer,w.outer)
        ???
    }
    ???
}

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

def generateSystemToTest(
    signals: Int,
    videoShots: Int,
    workstations: Int,
    signal2Shots: Int,
    shots2WorkStations: Int,
    displayLimits: (Int, Int)
): TestedSystem0 =
  // Nodes
  val signalNodes = (1 to signals).map(_ => Sig(UUID.randomUUID()))
  val shotsNodes = (1 to videoShots).map(_ => Video(UUID.randomUUID()))
  val workstationNodes = (1 to workstations).map(_ =>
    Workstation(
      UUID.randomUUID(),
      Random.between(displayLimits._1, displayLimits._2)
    )
  )

  // Edges between Signals and VideoShots
  val svEdges = for {
    s <- signalNodes
    v <- pick(signal2Shots, shotsNodes.length).map(shotsNodes)
  } yield s ~> v

  // Edges between VideoShots and Workstations
  val vwEdges = for {
    v <- shotsNodes
    w <- pick(shots2WorkStations, workstationNodes.length).map(workstationNodes)
  } yield v ~> w
  TestedSystem0(
    Graph.from[TestGraphNode, DiEdge[TestGraphNode]](
      signalNodes ++ shotsNodes ++ workstationNodes,
      svEdges ++ vwEdges
    )
  )

@main def graphTests(): Unit =
  val system = generateSystemToTest(
    signals = 10000,
    videoShots = 1000,
    workstations = 20,
    signal2Shots = 5,
    shots2WorkStations = 10,
    displayLimits = (5, 20)
  )
  val systemCopy = TestedSystem0(system.original)
  val algorithmSteps = SequencialAlgorithm0.testingSequence(system)
  val print = algorithmSteps.mkString("\n")
  println(print)
  println(algorithmSteps.length - 1)
  // val greedySteps = SimpleGreedyAlgorithm0.testingSequence(systemCopy)
  // val out = system.original.render(scalax.collection.ToString.SetElemsOnSeparateLines(), " ")
  // println(out)
