package v2


import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

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