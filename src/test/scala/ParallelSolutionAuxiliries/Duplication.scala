import v2.ParallelSolutionAuxiliaries
import scalax.collection.immutable.Graph
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import v2.GlobalAuxiliaries.showNodesSuccessors
import v2.SystemModel
import v2.GlobalAuxiliaries.addSource
import v2.GlobalAuxiliaries.addSink
import v2.ParallelSolutionAuxiliaries.duplicateWorkstations
import v2.DataToTestOn



class DuplicationTests extends munit.FunSuite {
  test("duplication working") {
    val model = DataToTestOn.Simplest.model
    val SystemModel(
      graph, 
      signals, 
      videoshots, 
      workstations, 
      workstationDisplays) = model

    val (withDuplication, ws, wsMap) = duplicateWorkstations(graph, workstations, workstationDisplays)

    assert(ws.length == workstations.length, "copy length must be equal")
    assert(workstationDisplays.size == wsMap.size)
    given Graph[Int, WDiEdge[Int]] = withDuplication

    // val zippedWorkstations = workstations.zip(ws)
    // println(zippedWorkstations)
    //   zippedWorkstations.map{ case (wn, wo)=>
    //   val oldPreds = (graph get wo).diPredecessors.map(_.outer)
    //   val newPreds = (graph get wn).diPredecessors.map(_.outer)
    //   (oldPreds, newPreds)
    // }

    //assert(connectionCheck, "newly created workstations are not connected to old predocessors")
  }
}
