package v2
import v2.GlobalAuxiliaries.*

import scala.concurrent.ExecutionContext

def testAndReport(
                          model:SystemModel,
                          problemSolutions: Iterable[ProblemSolution[SystemModel, ?, ?, ?, ?, Iterable[Action]]]
                        ): LazyList[String] =
  given ExecutionContext = ExecutionContext.global

  val SystemModel(_,_,_,_,_,
    SystemModel.SystemModelProps(signals, videoshots, workstations, signal2Shots, shot2Workstations, displayLimits)
  ) = model

  val totalPairs = SequentialTestingSolution.solveProblem(model).size
  val displaysTotal = model.workstationDisplays.values.sum
  val results = for{
    problemSolution<-problemSolutions
    ts0 = System.currentTimeMillis()
    resultTestingScenario = problemSolution.solveProblemParallel(model)
    ts1 = System.currentTimeMillis()

    runDuration = ts1 - ts0
    (displaysMinimum, displaysMaximum) = displayLimits
    steps = resultTestingScenario.size

    rMetric = (totalPairs + .0) / steps
    rRelation = rMetric / displaysTotal
  } yield s"$signals,$videoshots,$workstations,$signal2Shots,$shot2Workstations,$displaysMinimum,$displaysMaximum,$displaysTotal,$runDuration,$steps,$totalPairs,$rMetric,$rRelation"

  LazyList.from(results)


def massTest() =
  given ExecutionContext = ExecutionContext.global
  initializeIdGeneratorIfNot()

  val systemModel: SystemModel = GlobalAuxiliaries.generateSystemModel(
    signals = 1000,
    videoshots = 100,
    workstations = 10,
    signal2Shots = 12,
    shot2Workstations = 2,
    displayLimits = (2, 5)
  )

  writeGraphToGraphML(
    systemModel.graph,
    os.temp.dir(),
    s"${System.currentTimeMillis()}_test",
    systemModel.signals.toSet,
    systemModel.videoshots.toSet,
    systemModel.workstations.toSet
  )
  val ts0 = System.currentTimeMillis()
  val resultTestingScenario = FlowParallelTestingSolution.solveProblemParallel(systemModel)
  val ts1 = System.currentTimeMillis()

  println(resultTestingScenario.zipWithIndex.map((e, i)=> s"$i\t$e").mkString("\n"))
  println( s"Time passed = ${ts1 - ts0} millis")


@main def testSps() =
  massTest()

@main def testParallel() =
  initializeIdGeneratorIfNot()
  def printScenariosToCompare(model: SystemModel):Unit =
    val scenarioParallel = FlowParallelTestingSolution.solveProblemParallel(model)(using ExecutionContext.global)
    val scenarioSequential = SequentialTestingSolution.solveProblem(model)

    println("Model:")
    println(model)

    println("Sequential scenario:")
    println(scenarioSequential.mkString("\n"))

    println("Parallel scenario:")
    println(scenarioParallel.mkString("\n"))
    println("#"*80)

  val generated = generateSystemModel(
    signals = 1000,
    videoshots = 100,
    workstations = 10,
    signal2Shots = 12,
    shot2Workstations = 2,
    displayLimits = (2, 5)
  )
  val str =
    """
      |generateSystemModel(
      |    signals = 1000,
      |    videoshots = 100,
      |    workstations = 10,
      |    signal2Shots = 12,
      |    shot2Workstations = 2,
      |    displayLimits = (2, 5)
      |  )
      |""".stripMargin
  println(s"results for next model:\n$str")
  printScenariosToCompare(generated)

@main def testCSV() =
  initializeIdGeneratorIfNot()
  val header = "signals, videoshots, workstations, signal2Shots, shot2Workstations, displaysMinimum, displaysMaximum, displaysTotal, scenario generation time, steps, total_pairs, rMetric, rLimit, rRelation"
  val result = testAndReport(
    generateSystemModel(
      signals = 1000,
      videoshots = 100,
      workstations = 10,
      signal2Shots = 12,
      shot2Workstations = 2,
      displayLimits = (2, 5)
    ),
    Seq(SequentialTestingSolution, FlowParallelTestingSolution)
  )
  println(result.toVector.mkString("\n"))

