package com.example

import com.example.Chapter4.MySome
import com.example.Chapter5._

object Chapter6 extends App {
  type MyState[S,+A] = S => (A,S)
  type Rand[+A] = MyState[RNG, A]

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class State[S, +A](run: MyState[S,A]){

  }
  object State {
    def unit[S,A](a: A): MyState[S,A] = s => (a, s)
    def map[S,A,B](s: MyState[S,A])(f: A => B):MyState[S,B] = ss => {
      val (a, _) = s(ss)
      (f(a), ss)
    }
    def flatMap[S,A,B](f: MyState[S,A])(g: A => MyState[S,B]): MyState[S,B] = {s =>
      val (a, ss) = f(s)
      val gg =  g(a)
      gg(ss)
    }
    def map2[S,A,B,C](sa: MyState[S,A], sb: MyState[S,B])(f: (A,B) => C): MyState[S,C] = flatMap(sa)({a =>
      flatMap(sb)({b =>
        {s => (f(a,b), s)}
      })
    })
    def sequence[S,A](fs: List[MyState[S,A]]): MyState[S,List[A]] = s => {
      fs.foldLeft((s, List[A]()))({(ss, aa) =>
        val (oldState, oldList) = ss
        val (newA, newState) = aa(oldState)
        val newList = oldList :+ newA
        (newState, newList)
      }).swap
    }
  }
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  //coins on the left, candies on the right
  def simulateMachine(inputs: List[Input]): MyState[Machine, (Int, Int)] = {initialState =>
    val finalState = inputs.foldLeft(initialState)({(stateSoFar, input) =>
      stateSoFar.candies match {
        case numCandies if numCandies <= 0 =>
          stateSoFar
        case _ => stateSoFar.locked match {
          case true =>
            input match {
              case Turn => stateSoFar
              case Coin => Machine(false, stateSoFar.candies, stateSoFar.coins + 1)
            }
          case false =>
            input match {
              case Turn => Machine(true, stateSoFar.candies - 1, stateSoFar.coins)
              case Coin => stateSoFar
            }
        }
      }
    })
    ((finalState.coins, finalState.candies), finalState)
  }



  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, _) = s(rng)
    (f(a), rng)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, gen) = rng.nextInt
    n match {
      case Int.MinValue => (-1 * (Int.MinValue + 1), gen)
      case neg if neg < 0 => (-1 * neg, gen)
      case pos => (pos, gen)
    }
  }
  def double(rng: RNG): (Double, RNG) = {
    val (integer, gen) = nonNegativeInt(rng)
    (integer.toDouble / Int.MaxValue.toDouble, gen)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intResult, gen1) = rng.nextInt
    val (doubleResult, gen2) = double(gen1)
    ((intResult, doubleResult), gen2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val _temp = intDouble(rng)
    (_temp._1.swap, _temp._2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doubleResult1, gen1) = double(rng)
    val (doubleResult2, gen2) = double(gen1)
    val (doubleResult3, gen3) = double(gen2)
    ((doubleResult1, doubleResult2, doubleResult3), gen3)
  }
  def ints(count: Int)(implicit rng: RNG): (List[Int], RNG) = {
    val listOfInts: List[(Int, RNG)]= MyStream.unfold(rng)({s =>
      val (newInt, newGen) = s.nextInt
      MySome((newInt, newGen),newGen)
    }).take(count).toList
    listOfInts.foldLeft(List[Int](), rng)({(aggListPair, itemPair) =>
      (aggListPair._1.::(itemPair._1), itemPair._2)
    })
  }
  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)({x =>
      x - x % 2
    })
  }
  def mapDouble: Rand[Double] = {
    map(nonNegativeInt)({x =>
      x.toDouble / Int.MaxValue.toDouble
    })
  }
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    {rng =>
      val (a, genA) = ra(rng)
      val (b, genB) = rb(genA)
      (f(a,b), genB)
    }
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)({(a, b) => (a, b)})
  val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    {rng =>
      fs.foldLeft((rng, List[A]()))({(oldRng, nextRng) =>
        val (a, newRngA) = nextRng(oldRng._1)
        (newRngA, oldRng._2 :+ a)
      }).swap
    }
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, genA) = f(rng)
    val gg: Rand[B] = g(a)
    gg(genA)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)({i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0){
      {rng => (mod, rng)}
    } else {
      nonNegativeLessThan(n)
    }
  })
  def randomRange(start: Int, endExclusive: Int): Rand[Int] = flatMap(double)({doubleVal =>
    val number = (start + (endExclusive - start) * doubleVal).floor.toInt
    val result: Rand[Int] = {rng => (number, rng)}
    result
  })
  def mapf[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)({a =>
    {rng => (f(a), rng)}
  })
  def map2f[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)({a =>
    flatMap(rb)({b =>
      {rng =>  (f(a,b), rng)}
    })
  })
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)


  implicit val generator = SimpleRNG(190)
  val myNonNeg = nonNegativeLessThan(25)
  println(s"myNonNegLess: ${myNonNeg(generator)}")
  val listOfRands = List.fill[Rand[Double]](15)(double)
  println("random List of doubles::   "+sequence(listOfRands)(generator))
  println("random List of rangedInts::   "+sequence(List.fill[Rand[Int]](65)(randomRange(57,89)))(generator))
  val anInt: Rand[Int] = _.nextInt
  println(s"Double Value: ${double(generator)}")
  println(s"List of randInts: ${ints(25)}")
  println(anInt(generator))
  println(mapDouble.apply(generator)._1)
}
