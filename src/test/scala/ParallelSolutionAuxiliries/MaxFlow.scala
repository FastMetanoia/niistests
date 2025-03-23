package ParallelSolutionAuxiliries


import v2.SystemModel
import v2.DataToTestOn
import v2.ParallelSolutionAuxiliaries.*
import v2.GlobalAuxiliaries.*


class MaxFlow extends munit.FunSuite{
  test("minimal mf"){
    initializeIdGeneratorIfNot()
    val SystemModel(graph, signals, videoshots, workstations, workstationDisplays) =
      DataToTestOn.Minimal.model
    val (source, sink, graphToTest) = addSourceAndSink(graph, signals, workstations, workstationDisplays)
    val resultFlow = jMaxFlow(graphToTest, source, sink)
    val throughput = (resultFlow get source).edges.map(_.outer).filter(_.source == source).map(_.weight).sum
    assertEqualsDouble(throughput, 1d, 0.00001, s"1->1->1 flow must be 1, but it is $throughput")
  }
  test("simple1"){
    initializeIdGeneratorIfNot()
    val SystemModel(graph, signals, videoshots, workstations, workstationDisplays) =
      DataToTestOn.Simple.m1
    val (source, sink, graphToTest) = addSourceAndSink(graph, signals, workstations, workstationDisplays)
    val resultFlow = jMaxFlow(graphToTest, source, sink)
    val throughput = (resultFlow get source).edges.map(_.outer).filter(_.source == source).map(_.weight).sum
    assertEqualsDouble(throughput, 3d, 0.00001, s"flow must be 3, but it is $throughput")
  }
  test("simple2"){
    initializeIdGeneratorIfNot()
    val SystemModel(graph, signals, videoshots, workstations, workstationDisplays) =
      DataToTestOn.Simple.m2
    val (source, sink, graphToTest) = addSourceAndSink(graph, signals, workstations, workstationDisplays)
    val resultFlow = jMaxFlow(graphToTest, source, sink)
    val throughput = (resultFlow get source).edges.map(_.outer).filter(_.source == source).map(_.weight).sum
    assertEqualsDouble(throughput, 3d, 0.00001, s"flow must be 3, but it is $throughput")
  }
  test("not full"){
    val SystemModel(graph, signals, videoshots, workstations, workstationDisplays) =
      DataToTestOn.Simple.notFullModel
    val (source, sink, graphToTest) = addSourceAndSink(graph, signals, workstations, workstationDisplays)
    val resultFlow = jMaxFlow(graphToTest, source, sink)
    val throughput = (resultFlow get source).edges.map(_.outer).filter(_.source == source).map(_.weight).sum
    assertEqualsDouble(throughput, 2d, 0.00001, s"flow must be 2, but it is $throughput")
  }
}
