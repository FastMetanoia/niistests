package v2
import v2.GlobalAuxiliaries.*
import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory

import scala.concurrent.ExecutionContext

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
  val resultTestingScenario = SimpleParallelTestingSolution.solveProblemParallel(systemModel)
  val ts1 = System.currentTimeMillis()

  println(resultTestingScenario.zipWithIndex.map((e, i)=> s"$i\t$e").mkString("\n"))
  println( s"Time passed = ${ts1 - ts0} millis")


@main def testSps() =
  //println(os.rel.toNIO.toAbsolutePath)
  massTest()

@main def testParallel() =
  initializeIdGeneratorIfNot()
  def printScenariosToCompare(model: SystemModel):Unit =
    val scenarioParallel = SimpleParallelTestingSolution.solveProblemParallel(model)(using ExecutionContext.global)
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


//  println("m1:")
//  printScenariosToCompare(DataToTestOn.Simple.m1)
//  println("m2:")
//  printScenariosToCompare(DataToTestOn.Simple.m2)
//  println("not full:")
//  printScenariosToCompare(DataToTestOn.Simple.notFullModel)


