package v2


import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.immutable
import scalax.collection.immutable.Graph
import v2.ParallelSolutionAuxiliaries.{PreparedGraph, ProcessedGraph}

object SequentialTestingSolution
    extends ProblemSolution[
      SystemModel,
      (Int, SystemModel),
      (Int, SystemModel),
      Set[(Int, Int, Int)],
      Set[Action],
      Iterable[Action]
    ] {
  override def decomposeProblem(
      domainSystemModel: SystemModel
  ): Seq[(Int, SystemModel)] =
    domainSystemModel.signals.map(s => (s, domainSystemModel))

  override def transformToCalculationModel(
      problemPart: (Int, SystemModel)
  ):(Int, SystemModel) = problemPart

  override def calculateModel(
                               calculationInput: (Int, SystemModel)
  ): Set[(Int, Int, Int)] =
    val (signal, model) = calculationInput
    // Перебор всех пар сигнал-видеокадр c первой попавшейся рабочей станцией.
    (model.graph get signal).diSuccessors
      .map(v => signal -> v)
      .map((s, v) => (s, v.outer, (model.graph get v).diSuccessors.head.outer))
      

  override def interpretModel(
      calculationResult: Set[(Int, Int, Int)]
  ): Set[Action] =
    calculationResult.map { singleResult =>
      // Распаковка значений
      val (s, v, w) = singleResult
      // Формирование действия тестирования
      Action(signals = Set(s), Map(v -> w))
    }

  override def aggregateResults(domainResults: Iterable[Set[Action]]): Seq[Action] =
    domainResults.flatten.toSeq
}