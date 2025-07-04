


import org.jgrapht
import org.jgrapht.*
import org.jgrapht.graph.*

import java.net.URI
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.Random

// jGraphT example
@main def jGraphTest():Unit =
  val g = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])
  val google = new URI("http://www.google.com")
  val wikipedia = new URI("http://www.wikipedia.org")
  val jgrapht = new URI("http://www.jgrapht.org")

  // add the vertices
  g.addVertex(google)
  g.addVertex(google)
  g.addVertex(wikipedia)
  g.addVertex(jgrapht)

  // add edges to create linking structure
  g.addEdge(jgrapht, wikipedia)
  g.addEdge(google, jgrapht)
  g.addEdge(google, wikipedia)
  g.addEdge(wikipedia, google)

  val start = g.vertexSet().asScala
    .find(uri => uri.getHost.equals("www.jgrapht.org")).get
  println(start)
  println(g)

@main def zipTest():Unit =
  def cycledWorkstations(workstations: Seq[Int], i: Int = 0): LazyList[Int] =
    workstations(i) #:: cycledWorkstations(workstations, (i + 1) % workstations.length)

  println(s"cycledWorkstations(Seq(1,2,3)).take(10).toVector = ${cycledWorkstations(Seq(1,2,3)).take(10).toVector}")

@main def eqTest():Unit = {
//
//  val videoshots = Seq(
//    VideoShot(UUID.randomUUID()),
//    VideoShot(UUID.randomUUID()),
//    VideoShot(UUID.randomUUID()),
//    VideoShot(UUID.randomUUID())
//  )
//  val signals = Seq(
//    Signal(UUID.randomUUID()),
//    Signal(UUID.randomUUID()),
//    Signal(UUID.randomUUID())
//  )
//
//  val mapping:SignalShotMapping = Map(
//    signals(0) -> videoshots.toSet,
//    signals(1) -> videoshots.take(3).toSet,
//    signals(2) -> videoshots.takeRight(2).toSet
//  )
//
//
//  val arms = Set(
//    ARM(
//      UUID.randomUUID(),
//      Set(Display(UUID.randomUUID()), Display(UUID.randomUUID())),
//      Set(videoshots(0), videoshots(1)),
//      Set(signals(0), signals(1)),
//    ),
//    ARM(
//      UUID.randomUUID(),
//      Set(Display(UUID.randomUUID())),
//      Set(videoshots(2), videoshots(3)),
//      Set(signals(0), signals(1), signals(2))
//    )
//  )


  def pick(n: Int, rangeLimit: Int) =
    if (n > rangeLimit)
      throw IllegalArgumentException(
        s"An attempt to pick $n unique indices from 0 to $rangeLimit range"
      )

    def pickDifferent(chosen: Seq[Int], rangeLimit: Int): Int =
      val candidate = Random.nextInt(rangeLimit)
      if (!chosen.contains(candidate)) candidate
      else pickDifferent(chosen, rangeLimit)

    (1 to n).foldLeft(Seq.empty[Int])((chosen, _) =>
      chosen.appended(pickDifferent(chosen, rangeLimit))
    )


  //case class ARM(id:ID, ds:Set[Display], vs:Set[VideoShot], ss:Set[Signal])
  case class TestedSystem(arms: Set[ARM], signals: Set[Signal], videoshots: Set[VideoShot], mapping: SignalShotMapping)

  def generateARMS(armN: Int,
                   sigN: Int,
                   vidN: Int,
                   displayRange: (Int, Int),
                   s2vN: Int,
                   v2armRange: (Int, Int)
                  ): TestedSystem = {
    val signals = for (_ <- 0 to sigN) yield Signal(UUID.randomUUID())
    val videoshots = for (_ <- 0 to sigN) yield VideoShot(UUID.randomUUID())
    val mapping: SignalShotMapping = signals.map { s =>
      s -> pick(s2vN, vidN).map(videoshots).toSet
    }.toMap
    val arms = for {
      arm <- 0 to armN
      r = Random.between(v2armRange._1, v2armRange._2)
      videoShots = pick(r, vidN).map(videoshots).toSet
      displayNumber = Random.between(displayRange._1, displayRange._2)
      displays = (0 to displayNumber).map(_ => Display(UUID.randomUUID())).toSet
      id = UUID.randomUUID()
      signals = mapping.filter { case (s, vs) => vs.intersect(videoShots).nonEmpty }.keySet
    } yield ARM(id, displays, videoShots, signals)
    TestedSystem(arms.toSet, signals.toSet, videoshots.toSet, mapping)
  }

  def indexShowMap[T](seq:Seq[T], prefix:String="", l:String="(", r:String = ")"):Map[T, String] =
    seq.zipWithIndex.map{
      case (x, i)=> x-> (prefix + l + i + r)
    }.toMap

  def formatCluster(cluster:VCluster, signals:Seq[Signal], videoshots: Seq[VideoShot]):String = {
    val signalsIndexMapping = indexShowMap(signals, "s")
    val videoshotsIndexMapping = indexShowMap(videoshots, "v")
//    cluster.vs.tapEach(println)
//    videoshotsIndexMapping.tapEach(println)
    s"VCluster(${cluster.k}, ${cluster.vs.map(videoshotsIndexMapping)}, ${cluster.ss.map(signalsIndexMapping)})"
  }
  
  def formatBlock(sb:ScenarioBlock, arms:Seq[ARM], signals:Seq[Signal], videoshots:Seq[VideoShot]):String = {
    val armIndexMap = indexShowMap(arms, "arm")
    val videoshotIndexMap = indexShowMap(videoshots, "v")
    val signalIndexMap = indexShowMap(signals, "s")
    val stepsString = sb.steps.map { s =>
        s"Emit and test signal: ${signalIndexMap(s)}"
        //s"Step($confId:${armIndexMap(arm)},${videoshotIndexMap(videoShot)},Signals(${ss.map(signalIndexMap).mkString(", ")})"
    }.mkString("\n")
    val configurationPrefixString = s"SetConfiguration(${sb.videoshotsConfiguration.id},"
    val configurationMappingsString = sb.videoshotsConfiguration.vs.map {
      case (arm, vs) =>
        armIndexMap(arm) + "->" + vs.map(videoshotIndexMap).mkString("V{", ", ", "}")
    }
    val configurationPostfixString = ")"
    val confShow = configurationPrefixString + configurationMappingsString + configurationPostfixString

    confShow + "\n" + stepsString
  }
  
  {
    val generated = generateARMS(
      armN = 3,
      sigN = 10,
      vidN = 3,
      displayRange = (2, 5),
      s2vN = 2,
      v2armRange = (1, 3)
    )
    //1. выбрать k = Sum(|Di|) Множество проверенных кластеров (Tested) - пустое
    //2. Если k == 0 перейти к пункту (8)
    //3. Найти все кластеры размера k, не являющиеся подмножествами* кластеров из Tested
    //4. Полученные кластеры отфильтровать: оставить те, которые могут быть открыты на дисплеях.
    //5. Добавить получившиеся кластеры в множество Tested
    //6. k = k-1
    //7. Перейти к пунтку 2

    //8. Из множества кластеров Tested сформировать шаги алгоритма. Для каждого кластера:
    //8.1 Первый шаг - конфигурирование системы.
    //8.2 Шаги после первого - последовательная имитация и тестирование сигналов

    def calcK(arms:Set[ARM]):Int = arms.map(_.ds.size).sum
    val result = algLoop(
      calcK(generated.arms),
      generated.arms,
      generated.mapping
    )
    println(result.map(formatCluster(_, generated.signals.toSeq, generated.videoshots.toSeq)).mkString("\n"))
    println("#" * 90)
    println(generated.videoshots.map(_.id).mkString("\n"))
    println(generated.mapping.flatMap(p=>p._2.map(p2=>s"${p._1.id}:${p2.id}")).mkString("\n"))
  }


}






//sealed trait Value
//case class Is(x:Int) extends Value
//case class Js(x:Int) extends Value
//
//case class MyEdge(from:Value, to:Value) extends AbstractDiEdge[Value](from, to)
//
//@main def gTests():Unit =
//    val g:Graph[Value, MyEdge] =
//        Graph.from(
//            Seq(Js(1), Is(2)),
//            Seq(MyEdge(Js(1), Is(2)))
//        )
//
//    println(g)
//
//    val fn = g.nodes.find{ _.outer.isInstanceOf[Is]}
//    println(fn)
//    val cn = g.nodes.collectFirst{
//        case n:g.NodeT if n.outer.isInstanceOf[Is] => n.outer
//    }
//    println(cn)
//
//    println(Seq(1,2,3,4,5).sortBy(x=>x))
//
//@main def testMain():Unit =
//  println((1 to 3).map(x=>x))
//  val g = Graph.from[Int, WDiEdge[Int]](Seq(1,2,3), Seq(1~>2 % 1, 2~>3 % 2, 3~>1 % 2))
//  // modification
//  val ne = g.map(
//    fNode = _.outer,
//    fEdge = (edge, _, _) =>
//      val s:~>d % w = edge.outer
//      s ~> d % (w + 1)
//  )
//
//  // println(g.addOne(1~>3 % 3))
//  println((g get 1).edges.filter(e=>e.outer.source == 1).map(e=>e.outer.weight).sum)