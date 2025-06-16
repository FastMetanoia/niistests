package v2
import org.antlr.v4.runtime.atn.RuleStartState
import v2.GlobalAuxiliaries.*
import v2.SystemModel.SystemModelProps
import v2.TestProvider.{makeDataGenerator, testAndReport}

import scala.concurrent.ExecutionContext
import scala.util.Random


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

val algorithms = Seq(
  SequentialTestingSolution,
  GreedyParallelTestingSolution,
  FlowParallelTestingSolution,
  CliqueParallelTestingSolution
)

@main def testCSV() =


  initializeIdGeneratorIfNot()

  val stationaryData = makeDataGenerator(
    SystemModel.SystemModelProps(
      signals = 1000,
      videoshots = 100,
      workstations = 10,
      signal2Shots = 12,
      shot2Workstations = 2,
      displayLimits = (2, 5)
    ),
    identity
  )

  TestProvider.testAndWrite(stationaryData.take(100), algorithms, "StationaryTest")

def overallGrowthExperiment() = {
  initializeIdGeneratorIfNot()
  val startState = SystemModel.SystemModelProps(
    signals = 1000,
    videoshots = 100,
    workstations = 10,
    signal2Shots = 12,
    shot2Workstations = 2,
    displayLimits = (2, 5)
  )

  val data = makeDataGenerator(
    startState,
    s=>SystemModelProps(
      signals = s.signals + 100,
      videoshots = s.videoshots + 10,
      workstations = s.workstations + 1,
      signal2Shots = if(s.workstations % 10 < 2) s.signal2Shots + 2 else s.signal2Shots + 1,
      shot2Workstations = if(s.workstations % 10 < 2) s.shot2Workstations + 1 else s.shot2Workstations,
      displayLimits = (2, 5)
    )
  )
}
//  val header = "signals, videoshots, workstations, signal2Shots, shot2Workstations, displaysMinimum, displaysMaximum, displaysTotal, scenario generation time, steps, total_pairs, rMetric, rLimit, rRelation"
//  val result = testAndReport(
//    generateSystemModel(
//      signals = 1000,
//      videoshots = 100,
//      workstations = 10,
//      signal2Shots = 12,
//      shot2Workstations = 2,
//      displayLimits = (2, 5)
//    ),
//    Seq(SequentialTestingSolution, FlowParallelTestingSolution)
//  )
//  println(result.toVector.mkString("\n"))

