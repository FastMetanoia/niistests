package com.example

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

//Актор для вычисления интеграла
object IntegralActor {
    case class Calculate(start: Double, end:Double, numSteps: Int, replyTo: ActorRef[Result])
    case class Result(value: Double)

    //Поведение актора, который вычисляет интеграл
    def apply(): Behavior[Calculate] = Behaviors.receive { (context, message) =>
        val step = (message.end - message.start) / message.numSteps
        // Вычисляем сумму значений функции в промежуточных точках
        val intermediateSum = (1 until message.numSteps).map { i =>
            val x = message.start + i * step
            f(x)
        }.sum
        // Прибавляем значения на концах интервала (метод трапеций)
        val result = (f(message.start) + f(message.end)) / 2 + intermediateSum
        // Умножаем на шаг
        val integral = result * step

        context.log.info(s"Computed integral: $integral")
        message.replyTo ! Result(integral)
        Behaviors.same
    }

    // Функция для интегрирования (x^2)
    def f(x: Double): Double = x * x 
}

// Актор для суммирования частичных результатов
object SumActor{
    //добавим remainingResults для количества оставшихся вычислений
    case class AddResult(sum: Double, remainingResults: Int)
    case class FinalResult(result: Double)

    def apply(): Behavior[AddResult] = Behaviors.setup { (context) =>
        var totalSum = 0.0
        var resultsLeft = 0

        Behaviors.receiveMessage {
            case AddResult(sum, remainingResults) => 
                totalSum += sum
                resultsLeft = remainingResults
                //Логируем промежуточный результат
                context.log.info(s"Partial sum: $sum, current total: $totalSum, remaining: $resultsLeft")
                // Если все результаты получены, выводим финальный результат
                if (resultsLeft == 0) {
                    context.log.info(s"Final result: $totalSum")
                }

                Behaviors.same
        }
    }
}

//Актор для управления процессом
object MainActor {
  case class StartCalculation(l: Double, r: Double, steps: Int)

  def apply(): Behavior[StartCalculation] = Behaviors.setup { (context) =>
    // Создание акторов для вычисления интеграла и суммирования
    val integralActor = context.spawn(IntegralActor(), "integralActor")
    val sumActor = context.spawn(SumActor(), "sumActor")

    def sendCalculation(
      start: Double, 
      stepSize: Double, 
      numSteps: Int, 
      replyTo: ActorRef[IntegralActor.Result]
    ): Unit = {
      integralActor ! IntegralActor.Calculate(start, start + numSteps * stepSize, numSteps, replyTo)
    }

    Behaviors.receiveMessage {
      case StartCalculation(l, r, steps) =>
        // Вычисление шага для каждого подактора
        val stepSize = (r - l) / steps
        // Количество акторов
        val numActors = 4
        var remainingResults = numActors

        (0 until numActors).foreach { i =>
          val start = l + i * ((r-l) / numActors) 
          val numSteps = steps / numActors
          // Актор для обработки результата других акторов
          val replyTo = context.spawn(Behaviors.receiveMessage[IntegralActor.Result] {
            case IntegralActor.Result(partialSum) =>
              remainingResults -= 1
              sumActor ! SumActor.AddResult(partialSum, remainingResults)
              Behaviors.same
          }, s"responseActor-${i}")
          // Отправляем вычисления на интеграцию
          sendCalculation(start, stepSize, numSteps, replyTo)
        }
        Behaviors.same
    }
  }
}

@main def Main(): Unit = {
  val system = ActorSystem(MainActor(), "main")
  system ! MainActor.StartCalculation(0, 5, 1000) //Результат интеграла x^2 от 0 до 5 41.6
}

object Trivials{
    // Ковариантный функтор
    trait Functor[F[_]]:
        def map[A,B](f:A=>B):F[A]=>F[B]
    
    // Пример:
    sealed trait MyOption[+X]
    // Видно, что MySome - сам по себе функтор. MySome[X] ~ Identity[X] ~ X
    case class  MySome[X](x:X) extends MyOption[X]
    case object MyNone extends MyOption[Nothing]

    // Доказательство, что MyOption - функтор
    object optFunctor extends Functor[MyOption]:
      override def map[A, B](f: A => B): MyOption[A] => MyOption[B] = 
        x=> x match
            case MySome(x) => MySome(f(x))
            case MyNone => MyNone

    // Функтор, игнорирующий второй параметр
    // Const[X,Y] ~ Identity[X] ~ X
    case class Const[X,Y](x:X)

    type Cons[Y] = Const[?, Nothing]
    // Штука довольно странная, но по сути - доказательство, что любой тип является всегда пустым контейнером для любого другого типа.
    // (Мы использовали это в доказательстве того, что любой обычный тип - сам по себе является ADT)
    object constFunctor extends Functor[Cons]:
      override def map[A, B](f: A => B): Cons[A] => Cons[B] = 
        x => x

    // Бифунктор
    trait BiFunctor[F[_,_]]:
        // map на обе функции
        def bimap[A1,A2,B1,B2](f:A1=>A2, g:B1=>B2):F[A1,B1]=>F[A2,B2]

        // map на левую функцию: фиксируем правый аргумент функтора, получаем функтоор на левый аргумент
        def lmap[A1,A2,B](f:A1=>A2):F[A1, B]=>F[A2, B] = bimap(f,identity)

        // И наоборот, map на правую фунуцию:
        def rmap[A,B1,B2](g:B1=>B2):F[A, B1]=>F[A, B2] = bimap(identity,g)
        
    // Обратите внимание, обычный бифунктор - ковариантен по обоим параметрам!

    // Примеры.
    // пара типов - произведение
    case class MyTuple2[X,Y](x:X, y:Y)

    // доказательство
    object tupleBiFunctor extends BiFunctor[MyTuple2]:
      override def bimap[A1, A2, B1, B2](f: A1 => A2, g: B1 => B2): MyTuple2[A1, B1] => MyTuple2[A2, B2] = 
        case MyTuple2(x,y) => MyTuple2(f(x), g(y))
    
    // Either - сумма типов
    sealed trait MyEither[+A,+B]
    case class MyLeft[A](a:A) extends MyEither[A,Nothing]
    case class MyRight[B](b:B) extends MyEither[Nothing, B]

    // доказательство
    object EitherBifunctor extends BiFunctor[MyEither]:
      override def bimap[A1, A2, B1, B2](f: A1 => A2, g: B1 => B2): MyEither[A1, B1] => MyEither[A2, B2] = 
        case MyLeft(a) => MyLeft(f(a))
        case MyRight(b) => MyRight(g(b))

    // Технически, можно всё это засунуть в объекты-компаньоны и проделать другие улучшайзенги, но мы здесь не ради них.


    // Контрвариантный функтор. Обратите внимание на направления стрелок.
    trait ContraFunctor[F[_]]:
        def contramap[A,B](f:A=>B):F[B]=>F[A]
    
    // С примерами сложнее, но попробуем. 
    // Пусть есть у нас фильтры. определим их вот так:

    trait Filter[X]:
        def filter(x:X):Boolean

    // Пусть мы даже экземпляр создали.
    object StringFilter extends Filter[String]:

      override def filter(x: String): Boolean = x.length % 2 == 0
    
    // А ещё у нас есть преобразование Double => String, для результата которого мы хотим использовать фильтр.
    def doubleToString(d:Double):String = d.toString()

    // При этом, нам по какой-то причине важно сохранить функторность фильтра. 
    // То есть, нам нужна функция Filter[String] => Filter[Double], 
    // а получить её мы хотим по функции Double=>String
    // Иными словами, такой фильтр будет контрвариантным функтором.

    // доказательство
    object FilterContrvariant extends ContraFunctor[Filter] :
      override def contramap[A, B](f: A => B): Filter[B] => Filter[A] = 
        oldFilter => new Filter[A] :
          override def filter(x: A): Boolean = oldFilter.filter(f(x))
    
    // Профунктор
    // Опять же, смотрим на направления стрелок
    trait Profunctor[F[_,_]]:
      // Вот тут честно, не помню, как обычно называют эту функцию
      def promap[A1,A2,B1,B2](f:A2=>A1, g:B1=>B2):F[A1,B1]=>F[A2, B2]
      // Профунктор контрвариантен по первому аргументу и ковариантен по второму
    
    // Пример: собственно, объект функции
    trait MyFunction[A,B]:
      def apply(a:A):B

    // доказательство
    object MyFunctionProfunctor extends Profunctor[MyFunction]:
      override def promap[A1, A2, B1, B2](f: A2 => A1, g: B1 => B2): MyFunction[A1, B1] => MyFunction[A2, B2] = 
        oldFunction => new MyFunction[A2, B2]:
          def apply(a: A2): B2 = g(oldFunction.apply(f(a))) // (g o oldFunction.apply o f)(a)
  
  // FunctorExample[X] ~ X
  case class FunctorExample[X](x:X)

  
  object FunctorFunctor extends Functor[FunctorExample] {
    override def map[A, B](f: A => B): FunctorExample[A] => FunctorExample[B] = 
      functor => FunctorExample(f(functor.x))
  }
}



object MP{
  //(A=>X) x (B=>X) x (C=>X) ~ (A + B + C)=>X
  def k[A,B,C,X](f1:(A=>X), f2:(B=>X), f3:(C=>X)):(A | B | C)=>X = 
    x=>
      x match
        case a: A => f1(a)
        case b: B => f2(b)
        case c: C => f3(c)
  
  
  def m[A,B,C,X](pm:(A | B | C)=>X):((A=>X), (B=>X), (C=>X)) = 
    (
      pm,
      pm,
      pm
    )
}

object Monoids{
  // Мы про них как-то говорили. 
  // Моноид - это множество, замкнутая на этом множестве двуместная операция 
  // и нейтральный элемент относительно этой операции
  // То есть, как-то так

  trait Monoid[A]:
    //Сам по себе тип A является множеством.
    //Нейтральный элемент
    def empty:A 
    //Двуменстная операция
    def combine(x:A, y:A):A
  
  // Зачем?
  // Ну, напрмер, свёртка коллекций

  def foldAll[A](collection:Iterable[A], mon:Monoid[A]):A = 
    collection.foldLeft(mon.empty)(mon.combine)

  // Почему не reduce? - Потому что фолдить можно и пустую коллекцию

  // К можалению, мы никак не проверяем, что empty - нейтральный.
  // Впрочем, всё лучше, чем каждый раз писать новый fold - безопаснее и расширяемее.

  // Что-то ещё? - Да!
  // Пара моноидов - моноид на множестве пар (По той-же логике, по которой мы строили бифункторы)

  class MonoidPair[A,B](monA:Monoid[A], monB:Monoid[B]) extends Monoid[(A,B)]:

    override def empty: (A, B) = (monA.empty, monB.empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (monA.combine(x._1,y._1), monB.combine(x._2, y._2))

  // С суммой это так работать не будет, так как придётся предусматривать случаи, когда левый и правый операнды - разных типов.
}









object Fun{

}