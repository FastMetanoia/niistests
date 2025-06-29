package v2

import v2.GlobalAuxiliaries.generateSystemModel
import v2.SystemModel.SystemModelProps

import scala.concurrent.ExecutionContext

object TestProvider:

  val csvHeader = "algorithm, signals, videoshots, workstations, signal2Shots, shot2Workstations, displaysMinimum, displaysMaximum, displaysTotal, scenario generation time, steps, total_pairs, rMetric, rLimit, rRelation\n"

  def makeDataGenerator(
                                startProps:SystemModelProps,
                                fNextProps:SystemModelProps=>SystemModelProps
                              ):LazyList[SystemModel] =
    def go(
            startProps:SystemModelProps,
            fNextProps:SystemModelProps=>SystemModelProps
          ):LazyList[SystemModelProps] = startProps#::go(fNextProps(startProps),fNextProps)

    go(startProps,fNextProps)
      .map(generateSystemModel)

  def testAndReport(
                     model: SystemModel,
                     problemSolutions: Iterable[ProblemSolution[SystemModel, ?, ?, ?, ?, Iterable[Action]]]
                   ): LazyList[String] = {
    given ExecutionContext = ExecutionContext.global

    val SystemModel(_, _, _, _, _,
    SystemModel.SystemModelProps(signals, videoshots, workstations, signal2Shots, shot2Workstations, displayLimits, s2sDistributionType)
    ) = model

    val totalPairs = SequentialTestingSolution.solveProblem(model).size
    val displaysTotal = model.workstationDisplays.values.sum
    for {
      problemSolution <- LazyList.from(problemSolutions)
      ts0 = System.currentTimeMillis()
      resultTestingScenario = problemSolution.solveProblemParallel(model)
      ts1 = System.currentTimeMillis()

      runDuration = ts1 - ts0
      (displaysMinimum, displaysMaximum) = displayLimits
      steps = resultTestingScenario.size

      rMetric = (totalPairs + .0) / steps
      rRelation = rMetric / displaysTotal
    } yield s"${problemSolution.name}, $signals,$videoshots,$workstations,$signal2Shots,$shot2Workstations,$displaysMinimum,$displaysMaximum,$displaysTotal,$runDuration,$steps,$totalPairs,$rMetric,$rRelation"
  }


  def testAndWrite(
                    data:LazyList[SystemModel],
                    problemSolutions: Iterable[ProblemSolution[SystemModel, ?, ?, ?, ?, Iterable[Action]]],
                    name:String
                  ):Unit =
    val path = os.pwd / "output" / (name + ".csv")
    os.write.over(path, csvHeader)
    println(csvHeader)
    for {
      d<-data
      resultString <- testAndReport(d, problemSolutions)
    } {
      println(resultString)
      os.write.append(path, resultString + "\n")
    }

    


