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
    signals = 10000,
    videoshots = 100,
    workstations = 20,
    signal2Shots = 10,
    shot2Workstations = 2,
    displayLimits = (2, 5)
  )

  writeGraphToGraphML(
    systemModel.graph,
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
  massTest()

@main def testParallel() =
  initializeIdGeneratorIfNot()
  val model = DataToTestOn.Simple.notFullModel
  val scenario = SimpleParallelTestingSolution.solveProblemParallel(model)(using ExecutionContext.global)
  println(model)

  println(scenario.mkString("\n"))
