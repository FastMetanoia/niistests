

import java.util.UUID
import scala.annotation.tailrec
//

type ID = UUID
type SignalShotMapping = Map[Signal, Set[VideoShot]] // условно говоря, наша бд.

case class Display(id:ID)

case class VideoShot(id:ID)

case class Signal(id:ID)

case class ARM(id:ID, ds:Set[Display], vs:Set[VideoShot], ss:Set[Signal])

case class VCluster(k:Int, vs:Set[VideoShot], ss:Set[Signal])

case class ScenarioValidationStep( configurationId:Int,
                                   arm:ARM,
                                  //display:Display,
                                  videoShot: VideoShot,
                                  ss:Set[Signal])
case class ScenarioBlock(videoshotsConfiguration: VideoshotsConfiguration, steps:Seq[Signal])

case class VideoshotsConfiguration(id:Int, vs:Map[ARM, Set[VideoShot]])

case class Scenario(steps:Seq[ScenarioBlock])

//Шаг 1. Сформировать список кластеров величины К.
def getClustersOfSize(k:Int, arms:Set[ARM], signalShotMapping: SignalShotMapping, oldClusters: Seq[VCluster] = Seq.empty):Set[VCluster] = {
  //Возьмём все сигналы, отображаемые на k или более видеокадрах.
  //Сформируем из них все возможные vcluster c одним сигналом и мощностью k
  val clusters = for {
    arm <- arms
    (s, videoShots) <- signalShotMapping
    if videoShots.size >= k
    vs <- videoShots.subsets(k)
    if arms.forall(testArm=> testArm.ds.size >= vs.intersect(testArm.vs).size)
    if !oldClusters.exists(c=> vs.subsetOf(c.vs) && c.ss.contains(s))
  } yield VCluster(k, vs, Set(s))

  def mergeClustersStep(clusters: Set[VCluster]): Set[VCluster] = {
    val newClusters = for {
      vc1 <- clusters
      vc2 <- clusters
      if vc1 != vc2
      if vc1.vs == vc2.vs
    } yield ((vc1, vc2), VCluster(vc1.k, vc1.vs, vc1.ss ++ vc2.ss))
    //return newClusters.map(_._2);
    newClusters.map(c => c._2) ++ clusters.filterNot(c => newClusters.exists(nc => nc._1._1 == c || nc._1._2 == c))
  }

  @tailrec
  def mergeClusters(clusters: Set[VCluster]): Set[VCluster] =
    val next = mergeClustersStep(clusters)
    if (next == clusters) clusters else mergeClusters(next)

  mergeClusters(clusters)
}

////Шаг 2. Сортировка
//def sortClusters(cs:Set[VCluster]):Seq[VCluster] = cs.toSeq.sortBy(c=>c.ss.size)

//Шаг 12. Запись
def writeScenario(steps:Seq[ScenarioBlock]):Unit =
  println(steps)

//Шаг 3. Сформировать шаги
//todo: имитировать СИГНАЛЫ, выкинуть видеокадры.
def getSteps(vClusters:Seq[VCluster], arms:Set[ARM]):Seq[ScenarioBlock] = {
  vClusters.zipWithIndex.map { case (c, i)=>
    val configuration = VideoshotsConfiguration(
      i,
      arms.map{ arm=>
        arm -> arm.vs.intersect(c.vs)
      }.toMap
    )
//    val steps = configuration.vs.flatMap {
//      case (arm, vs) =>
//        vs.map(v=> ScenarioValidationStep(i, arm, v, c.ss))
//    }
    ScenarioBlock(configuration, c.ss.toSeq)
  }
}

//val k = arms.flatMap(_.ds).size
def algLoop(k:Int, arms:Set[ARM], signalShotMapping: SignalShotMapping, clustersTested:Seq[VCluster] = Seq.empty):Seq[VCluster] =
  if(k == 0) clustersTested
  else
    algLoop(
      k - 1, 
      arms, 
      signalShotMapping, 
      clustersTested.appendedAll(getClustersOfSize(k, arms, signalShotMapping, clustersTested))
    )

enum ClusterRelation{
  case CONTAINS
  case PARTICIPATES
  case NONE
}

def calculateRelation(c1:VCluster, c2:VCluster):ClusterRelation =
  import ClusterRelation.*
  if (c1.vs.subsetOf(c2.vs)) PARTICIPATES
  else if (c1.vs.subsetOf(c2.vs)) CONTAINS
  else NONE



def reorganizeVClusters(clusters:Seq[VCluster]):Seq[VCluster] = {
  case class Pack(cluster:VCluster, subClusters:Seq[VCluster])

  def findPack(cluster:VCluster, clusters: Seq[VCluster], known:Seq[VCluster] = Seq.empty): Pack =
    import ClusterRelation.*
    clusters.foldLeft(Pack(cluster, known)) { case (Pack(cluster, subClusters), c) =>
      calculateRelation(cluster, c) match
        case ClusterRelation.CONTAINS => Pack(cluster, subClusters.appended(c))
        case ClusterRelation.PARTICIPATES => return findPack(c, clusters.filterNot(_ == c), known)
        case ClusterRelation.NONE => Pack(cluster, subClusters)
    }

  ???
}



//???

//def algorithmItself(arms:Set[ARM], signalShotMapping: SignalShotMapping):Unit = {
//  //Шаг 0. k = |D|
//  var k = arms.flatMap(_.ds).size
//
//  //Шаг 1. Сформировать список кластеров величины К.
//  val clusters = getClustersOfSize(k, arms, signalShotMapping, Seq.empty)
//  //Шаг 2. Сортировка
//  val sortedClusters = sortClusters(clusters)
//  //Шаг 3. Сформировать шаги
//  val steps = getSteps(sortedClusters, arms)
//  //Шаг 4. 𝑘 = 𝑘 − 1, ℎ = 1;
//  //Что такое h?
//
//  //h : раз уж мы хотим иметь под рукой последний кофниг, просто будем его хранить.
//  var lastConfig = steps.last.videoshotsConfiguration
//  k = k - 1
//  //Шаг 5. "Если 𝑘 > 0, то перейти к шагу 6, иначе перейти к шагу 12"
//  //var writeFlag = true
//  while (k > 0) {
//    //Шаг 6 (и 8)
//    val newClusters = getClustersOfSize(k, arms, signalShotMapping, clusters.toSeq)
//    if (newClusters.nonEmpty) {
//      //Шаги 8 - 10
//      //Шаг 11
//      k = k - 1
//    } else {
//      //Шаг 11
//      k = k - 1
//    }
//  }
//  //Шаг 12 Запись
//  writeScenario(steps)
//}

