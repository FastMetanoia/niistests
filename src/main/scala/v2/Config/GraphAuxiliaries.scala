package v2.Config

import org.jgrapht.graph.{DefaultWeightedEdge, SimpleDirectedWeightedGraph}
import org.jgrapht.graph.builder.GraphTypeBuilder
import scalax.collection.edges.labeled.WDiEdge
import scalax.collection.immutable.Graph
//import org.jgrapht.nio.graphml.GraphMLImporter
//import org.jgrapht.nio.graphml.*
import scalax.collection.edges.~>
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import v2.SystemModel
import v2.SystemModel.SystemModelProps

import scala.jdk.CollectionConverters.given
import java.io.FileReader
import scala.language.postfixOps
import scala.util.Try

object GraphAuxiliaries:
  extension(g:SimpleDirectedWeightedGraph[Int, DefaultWeightedEdge]){
    def asScalaxGraph:Graph[Int, WDiEdge[Int]] = Graph.from(
      g.vertexSet().asScala,
      g.edgeSet().asScala.map(edge => g.getEdgeSource(edge) ~> g.getEdgeTarget(edge) % g.getEdgeWeight(edge))
    )
  }

  def readFromGraphML(filePath:String):Graph[Int, WDiEdge[Int]] =
    val graph = SimpleDirectedWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    val fileReader = new FileReader(filePath)
    //new GraphMLImporter().importGraph(graph, fileReader)
    fileReader.close()
    graph.asScalaxGraph
    ???

  def SystemFromGraph(graph: Graph[Int, WDiEdge[Int]], displayMapping:Map[Int, Int]):Try[SystemModel] =
    // Если нет предшественников - значит сигнал
    val signals = graph.nodes.filter(!_.hasPredecessors)
    // Всё, что после сигнала - видеокадр
    val videoshots = signals.flatMap(_.diSuccessors)
    // Всё, что после видеокадра - рабочая станция
    val workstations = videoshots.flatMap(_.diSuccessors)

    Try{
      // Валидация графа
      // Непустые сигналы, видеокадры, рабочие станции
      if (signals isEmpty)                                throw new IllegalArgumentException("Graph has wrong shape: No signals detected")
      if (videoshots isEmpty)                             throw new IllegalArgumentException("Graph has wrong shape: No videoshots detected")
      if (workstations isEmpty)                           throw new IllegalArgumentException("Graph has wrong shape: No workstations detected")

      // У рабочих станций нет исходящих рёбер
      if (workstations.flatMap(_.diSuccessors).nonEmpty)  throw new IllegalArgumentException("Graph has wrong shape: Workstations have outgoing edges")

      // У всех сигналов и видеокадров есть исходящие рёбра
      if (signals.exists(!_.hasSuccessors))               throw new IllegalArgumentException("Graph has wrong shape: Signal has no successors")
      if (videoshots.exists(!_.hasSuccessors))            throw new IllegalArgumentException("Graph has wrong shape: Videoshot has no successors")

      val sigs = signals.map(_.outer).toSeq
      val vids = videoshots.map(_.outer).toSeq
      val works = workstations.map(_.outer).toSeq
      
      
      // Если всё ок
      SystemModel(
        graph,
        sigs,
        vids,
        works,
        displayMapping,
        SystemModelProps(
          sigs.size,
          vids.size,
          works.size,
          signals.head.diSuccessors.size,
          videoshots.head.diSuccessors.size,
          (displayMapping.values.min, displayMapping.values.max + 1)
        )
      )
    }
