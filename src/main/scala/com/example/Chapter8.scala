package com.example

import java.util.concurrent.{ExecutorService, Executors}

import Chapter6._
import com.example.Chapter4.MySome
import com.example.Chapter5.MyStream
import com.example.Chapter8.PropTrait.{FailedCase, SuccessCount}
import com.google.common.util.concurrent.ThreadFactoryBuilder

import scala.concurrent.ExecutionContext
import scala.util.Try

object Chapter8 extends App {
  println("hello")

  type TestCases = Int
//  type Result = Either[(FailedCase, SuccessCount), SuccessCount]

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  trait PropTrait {
    import PropTrait._

    def check: Result

    def ||(p: PropTrait): PropTrait = {
      val myP = this
      new PropTrait {
        override def check: Result = {
          myP.check match {
            case Passed => Passed
            case f @ Falsified(_, _) => p.check match {
              case Passed => Passed
              case ff @ Falsified(_, _) => f
            }
          }
        }
      }
    }

    def &&(p: PropTrait): PropTrait = {
      val myP = this
      new PropTrait {
        override def check: Result = {
          myP.check match {
            case f @ Falsified(_, _) => f
            case Passed => p.check match {
              case f @Falsified(_, _) => f
              case Passed => Passed
            }
          }
        }
      }
    }

  }
  case class Prop(run: (TestCases, RNG) => Result) extends PropTrait {
    override def check: Result = ???
  }



  case class Gen[A](sample: MyState[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen({x =>
        val (a, rng) = sample(x)
        f(a).sample(x)
      })
    }
    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap({integer =>
        Gen(State.sequence(List.fill(integer)(sample)))
      })
    }
//    def unsized: SGen[A] = {
//      SGen(_ => this)
//    }

  }

//  case class SGen[+A](forSize: Int => Gen[A]) {
//    def flatMap[B](f: A => SGen[B]): SGen[B] = {
//      SGen({fs =>
//        val ff: A => Gen[B] = {aa => f(aa).forSize(fs)}
//        forSize(fs).flatMap(ff)
//      })
//    }
//    def listOfN(size: SGen[Int]): SGen[List[A]] = {
//      SGen({trialInteger =>
//        forSize(trialInteger).listOfN(size.forSize(trialInteger))
//      })
//    }
//  }

  object Gen{
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(randomRange(start, stopExclusive))
    }
    def unit[A](a: => A): Gen[A] = {
      Gen(State.unit(a))
    }
    def boolean: Gen[Boolean] = {
      Gen(State.map(double)(_ > 0.5))
    }
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(g.sample)))
    }
    def listOf[A](a: Gen[A]): Gen[List[A]] = ???
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap({bool =>
        if(bool){
          g1
        } else {
          g2
        }
      })
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) =>
        val result = randomStream(as)(rng).zipAll(MyStream.from(0)).take(n).map {
          case (a, i) =>
            Try{
              if (f(a.getUnSafe())) {
                Passed
              } else {
                Falsified(a.toString, i.getUnSafe())
              }
            } match {
              case scala.util.Success(value) => value
              case scala.util.Failure(exception) =>
                val message = a match {
                  case MySome(theA) => buildMsg(theA, exception)
                  case _ => s"Value of A is missing.  Exception: $exception"
                }
                Falsified(message, i.getUnSafe())
            }
        }.find(_.isFalsified).getOrElse(Passed)
        result
    }

    def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] = MyStream.unfold(rng)(rng => MySome(g.sample(rng)))

    def buildMsg[A](s: A, e: Throwable): FailedCase =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  }

  object PropTrait {
    type FailedCase = String
    type SuccessCount = Int
  }
  println(Gen.listOfN(5, Gen.choose(1,10)).sample(SimpleRNG(2))._1)
  val streamGen = Gen.randomStream[List[Int]](Gen.listOfN(6, Gen.choose(5, 25)))(SimpleRNG(2))
  val randomLists = streamGen.take(5).toList
  println(randomLists)
//  import cats.implicits._
  ////  import scala.concurrent.{ Await, Future }
  ////  import scala.concurrent.duration._
  ////
  ////  val observerPool: ExecutorService = Executors.newFixedThreadPool(5, new ThreadFactoryBuilder().setNameFormat("Observer-processes").build())
  ////  val observerExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(observerPool)
  ////  val f: Int => Future[Unit] = i =>
  ////    Future {
  ////      Thread.sleep(1000)
  ////      println(i)
  ////    }(observerExecutionContext)
//  val _      = Await.ready((1 to 10).toList.traverse(f), 5.seconds)
}
