package v2


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

// Абстрактный класс для решения проблемы.
abstract class ProblemSolution[
    DomainSystemModel,
    DomainProblemPart,
    CalculationModelInput,
    CaclulcationModelOutput,
    DomainSolutionPart,
    DomainSolutionWhole
] {
  def decomposeProblem(
      domainSystemModel: DomainSystemModel
  ): Iterable[DomainProblemPart]
  def transformToCalculationModel(
      problemPart: DomainProblemPart
  ): CalculationModelInput
  def calculateModel(
      caclucaltionInput: CalculationModelInput
  ): CaclulcationModelOutput
  def interpretModel(
      calculationResult: CaclulcationModelOutput
  ): DomainSolutionPart
  def aggregateResults(
      domainResults: Iterable[DomainSolutionPart]
  ): DomainSolutionWhole

  def solveProblem(domainSystemModel: DomainSystemModel): DomainSolutionWhole =
    val calculatedSolutionParts = decomposeProblem(domainSystemModel)
      .map(transformToCalculationModel)
      .map(calculateModel)
      .map(interpretModel)
    aggregateResults(calculatedSolutionParts)

  def solveProblemParallel(domainSystemModel: DomainSystemModel)(using
      ExecutionContext
  ): DomainSolutionWhole =
    val resultParts = decomposeProblem(domainSystemModel).map(part =>
      Future(
        transformToCalculationModel
          .andThen(calculateModel)
          .andThen(interpretModel)(part)
      )
    )
    val futureResults = Future.sequence(resultParts).map(aggregateResults)
    Await.result(futureResults, Duration.Inf)
}