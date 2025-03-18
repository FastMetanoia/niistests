package ParallelSolutionAuxiliries


import v2.SystemModel
import v2.DataToTestOn
import v2.ParallelSolutionAuxiliaries.* 
import v2.GlobalAuxiliaries.addSource
import v2.GlobalAuxiliaries.addSink
import v2.GlobalAuxiliaries.addSourceAndSink


class FordFulcersonMaxFlow extends munit.FunSuite{
  test("minimal"){
    val SystemModel(graph, signals, videoshots, workstations, workstationDisplays) = 
      DataToTestOn.Simplest.model
    val (source, sink, graphToTest) = addSourceAndSink(graph, signals, workstations, workstationDisplays)
    val resultFlow = fordFulkersonMaximumFlow(graph, source, sink)
    val throughput = (resultFlow get source).edges.map(_.outer).filter(_.source == source).map(_.weight).sum
    assertEqualsDouble(throughput, 1d, 0.00001, s"1->1->1 flow must be 1, but it is $throughput")
  }
}
