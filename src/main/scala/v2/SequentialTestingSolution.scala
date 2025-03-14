package v2

import java.util.concurrent.atomic.AtomicLong
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

class SequentialTestingSolution(graph: Graph[Int, WDiEdge[Int]])
    extends ProblemSolution[
      SystemModel,
      Int,
      Int,
      Set[(Int, Int, Int)],
      Set[Action],
      Seq[Action]
    ] {

  override def decomposeProblem(
      domainSystemModel: SystemModel
  ): Seq[Int] =
    domainSystemModel.signals
  override def transformToCalculationModel(
      problemPart: Int
  ): Int = problemPart

  override def calculateModel(
      caclucaltionInput: Int
  ): Set[(Int, Int, Int)] =
    val signal = caclucaltionInput
    // Перебор всех пар сигнал-видеокадр c первой попавшейся рабочей станцией.
    (graph get signal).diSuccessors
      .map(v => signal -> v)
      .map((s, v) => (s, v.outer, (graph get v).diSuccessors.head.outer))

  override def interpreteModel(
      calculationResult: Set[(Int, Int, Int)]
  ): Set[Action] =
    calculationResult.map { singleResult =>
      // Распаковка значений
      val (s, v, w) = singleResult
      // Формирование действия тестирования
      Action(signals = Set(s), Map(v -> w))
    }

  override def agregateResults(
      domainResults: Iterable[Set[Action]]
  ): Seq[Action] = domainResults.flatten.toSeq
}