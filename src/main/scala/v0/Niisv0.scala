import java.util.UUID


// АРМ
case class AutomatedWorkstation(id:UUID, videoShots:Set[VideoShot], screensAmount:Int):
    override def hashCode(): Int = id.hashCode()
    override def equals(that: Any): Boolean = 
        that match
            case VideoShot(id, _, _) => id == this.id
            case _ => false
    
// Видеокадр
case class VideoShot(id:UUID, signals:Set[Signal], workstations:Set[AutomatedWorkstation]):
    override def hashCode(): Int = id.hashCode()
    override def equals(that: Any): Boolean = 
        that match
            case VideoShot(id, _, _) => id == this.id
            case _ => false

// Сигнал
case class Signal(id:UUID, shots:Set[VideoShot]):
    override def hashCode(): Int = id.hashCode()
    override def equals(that: Any): Boolean = 
        that match
            case VideoShot(id, _, _) => id == this.id
            case _ => false

// Тестируемые объекты - связи между сигналами и видеокадрами
case class Link(signal:Signal, shot:VideoShot)

//Все сигналы, кадры и АРМы
case class TestedSystem(signals:Set[Signal], shots:Set[VideoShot], workstations:Set[AutomatedWorkstation]):
    def links:Set[Link] = 
        for{
                shot <- shots
                signal <- shot.signals
            } yield Link(signal, shot)


// Обобщённое действие тестирования
case class UnifiedAction(shotsToSet:Map[AutomatedWorkstation, Set[VideoShot]], signalsToTest:Set[Signal], time:Int)
object UnifiedAction:
    val idle:UnifiedAction = UnifiedAction(Map.empty, Set.empty, 0)


// Трейт для алгоритмов, описываемых через обощённые действия
trait UnifiedActionTestingAlgorithm(protected val testTime:Int, protected val shotChangeTime:Int):
    def chooseAction(testedSystem:TestedSystem,linksLeft:Set[Link]):UnifiedAction

    def applyAction(links:Set[Link], action:UnifiedAction):Set[Link] = 
        val testedLinks = links
                .filter( link => action.signalsToTest.contains(link.signal))
                .filter( link => action.shotsToSet.valuesIterator.flatten.contains(link.shot))
        links -- testedLinks
    
    def actionSequence(testedSystem:TestedSystem):LazyList[(Set[Link], UnifiedAction)] = 
        def actionSequence(startLinks:Set[Link], actionDone:UnifiedAction):LazyList[(Set[Link], UnifiedAction)] = 
            val nextAction = chooseAction(testedSystem, startLinks)
            if(startLinks.nonEmpty)
                (startLinks, actionDone) #:: actionSequence(applyAction(startLinks, nextAction), nextAction)
            else 
                LazyList.empty
        
        actionSequence(testedSystem.links, UnifiedAction.idle)



// То что сейчас используется, я так понимаю.
class SequencialAlgorithm(tt:Int, sct:Int) extends UnifiedActionTestingAlgorithm(tt, sct):
  override def chooseAction(testedSystem: TestedSystem, linksLeft: Set[Link]): UnifiedAction = 
    val shot = linksLeft.head.shot
    val workstation = testedSystem.workstations.find(_.videoShots.contains(shot)).get
    val linksToEliminate = linksLeft.filter(_.shot == shot)
    val time = shotChangeTime + testTime * linksToEliminate.size
    UnifiedAction(
        Map(workstation->Set(shot)),
        linksToEliminate.map(_.signal),
        time
        )


def cartesianProduct[A,B](set1:Set[A], set2:Set[B]):Iterator[(A,B)] = for{a<-set1.iterator; b<-set2.iterator} yield (a,b)

// Жадный алгоритм с максимально простым парарллелизмом
class SimpleGreedyAlgorithm(tt:Int, sct:Int) extends UnifiedActionTestingAlgorithm(tt, sct):

  override def chooseAction(testedSystem: TestedSystem, linksLeft: Set[Link]): UnifiedAction = 


    // shots - кадры, для которых мы строим индекс
    // links - связи для этих кадров.
    def shotsAdjacencyIndex(shots:Set[VideoShot], links:Set[Link]):Seq[(VideoShot, Set[Signal])] = 
        val effectiveShots = shots.filter(shot=>links.exists(_.shot == shot))
        val shotPairs = for{
            s1<-effectiveShots
            s2<-effectiveShots
            if s1 != s2
            commonLinks = links.filter(link=> link.shot == s1 || link.shot == s2)
            s1Links = commonLinks.filter(_.shot == s1)
            s2Links = commonLinks.filter(_.shot == s2)
            commonSignals = cartesianProduct(s1Links, s2Links)
                .collect{case (Link(sig1,_), Link(sig2, _)) if sig1 == sig2 => sig1}
        } yield (s1, commonSignals)
        shotPairs
            .map((shot, signals)=> (shot, signals.toSet))
            .toSeq.sortBy( - _._2.size)
    
    def handleWorkstation(
        shotsChosen:Set[VideoShot], 
        signalsChosen:Set[Signal],
        workstation:AutomatedWorkstation,
        ):(AutomatedWorkstation, Set[VideoShot], Set[Signal]) = 
            // Видеокадры АРМа, не задействованые в на других АРМах
            val validShots = workstation.videoShots &~ shotsChosen
            // Сигналы АРМа, не задействованые на других АРМах
            val validSignals = validShots.flatMap(_.signals) &~ signalsChosen
            // Связки сигнал-кадр, которые можно тестровать на конкретном АРМе
            val validLinks = linksLeft
                .filter(link=> validShots.contains(link.shot))
                .filter(link=> validSignals.contains(link.signal))
            // кадры, отсортированные по количеству сигналов, смежных с сигналами с других видеокадров
            val index = shotsAdjacencyIndex(validShots, validLinks)
            // кадры для отображения на экранах
            val shots = index.map(_._1).take(workstation.screensAmount).toSet
            // сигналы для имитации на выбранных кадрах
            val signals = validLinks.filter(link=> shots.contains(link.shot)).map(_.signal)
            (workstation, shots, signals)
    
    val workstations = testedSystem.workstations
    val workstationsMapping = workstations.tail.foldLeft(
        Seq(handleWorkstation(Set.empty,Set.empty,workstations.head))
        ){(handledWorkstations, workstation)=>
            handledWorkstations.appended(handleWorkstation(
                handledWorkstations.flatMap(_._2).toSet,
                handledWorkstations.flatMap(_._3).toSet,
                workstation))}
    
    //Это горькая неправда из-за того, как именно работает applyUnifiedAction
    val time:Int = {
        val fstRun = shotChangeTime + testTime
        fstRun
    }

    UnifiedAction(
        workstationsMapping.map((workstation, shots, _)=>workstation->shots).toMap,
        workstationsMapping.flatMap(_._3).toSet,
        time
        )


trait Restriction[E,S]:
    def isRight(elem:E, system:S):Boolean
    def rectified(elem:E, system:S):(E,S)

sealed trait Amount
case class StrictAmount(n:Int) extends Amount
case class Persentage(x:Double) extends Amount

object Gen:
    def apply(
        singalsAmount:StrictAmount,
        shotsAmount:StrictAmount,
        automatedWorkstationsAmount:StrictAmount,
        signals2ShotsLinksAmount:Amount,
        shot2WorstationsLinksAmount:Amount,
        signalRestrictions:Seq[Restriction[Signal,TestedSystem]],
        shotRestrictions:Seq[Restriction[VideoShot,TestedSystem]],
        automatedWorkstationsRestrictions:Seq[Restriction[VideoShot,TestedSystem]]
    ):TestedSystem = {
        ???
    }



@main def testAlgorithm() = ???