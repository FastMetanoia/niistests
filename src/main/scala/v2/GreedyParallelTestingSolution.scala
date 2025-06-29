package v2

import scala.collection.mutable

object GreedyParallelTestingSolution extends ProblemSolution[
  SystemModel,
  (Int, SystemModel),
  (Int, Iterable[Int], SystemModel),
  Iterable[(Int, Set[(Int, Seq[Int])])],
  Iterable[Action],
  Iterable[Action]]{
  override def name: String = "Greedy"

  override def decomposeProblem(domainSystemModel: SystemModel): Iterable[(Int, SystemModel)] = domainSystemModel.signals.map(_->domainSystemModel)

  override def transformToCalculationModel(problemPart: (Int, SystemModel)): (Int, Iterable[Int], SystemModel) =
    val (sig, model) = problemPart
    (sig, (model.graph get sig).diSuccessors.map(_.outer), model)

  override def calculateModel(calculationInput: (Int, Iterable[Int], SystemModel)): Iterable[(Int, Set[(Int, Seq[Int])])] =
    val (sig, vs, model) = calculationInput

    val mutableWorkstationDisplays:mutable.Map[Int, Int] = mutable.Map.from(model.workstationDisplays)

    val availableVs = mutable.Set.from(vs)
    val wNodes = vs.flatMap(v => (model.graph get v).diSuccessors).toSet
    val wvsMap = for {
      wNode <- wNodes.filter(n=>mutableWorkstationDisplays(n.outer) > 0)
      effectiveVs = availableVs.intersect(wNode.diPredecessors.map(_.outer))
      w = wNode.outer
      displaysN = mutableWorkstationDisplays(w)
      vs = effectiveVs.take(displaysN).toSeq
    } yield {
      vs.foreach(availableVs.remove)
      mutableWorkstationDisplays(w) -= vs.size
      (w, vs)
    }
    if(availableVs.isEmpty)
      Seq((sig, wvsMap))
    else
      Seq((sig, wvsMap)).appendedAll(calculateModel((sig, availableVs, model)))



  override def interpretModel(calculationResult: Iterable[(Int, Set[(Int, Seq[Int])])]): Iterable[Action] =
    for {
      (sig, wvsMap) <- calculationResult
    } yield {
      val vwMap = wvsMap.flatMap {(w, vs)=> vs.map(v=> v->w)}
      //vwMap = vs.map(v => v -> w)
      Action(Set(sig), vwMap.toMap)
    }

  override def aggregateResults(domainResults: Iterable[Iterable[Action]]): Iterable[Action] =
    val result = domainResults.flatten
    println(result.mkString("\n"))
    result
}
