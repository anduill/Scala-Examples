package com.example

import scala.annotation.tailrec

object Chapter3 extends App{
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean = {
      index match {
        case i if i == (as.length - 1) => true
        case i =>
          ordered(as(i), as(i+1)) && loop(i+1)
      }
    }
    loop(0)
  }
  def curry[A,B,C](f: (A,B) => C): A => B => C ={
    {a =>
    {b =>
      f(a,b)
    }
    }
  }
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    {(a,b) =>
      f(a)(b)
    }
  }
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    {a =>
      f(g(a))
    }
  }
  val myList: List[Int] = 1 :: Nil
  println(myList)
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }
  def myLength[A](as: List[A]): Int = {
    foldRight(as, 0)({(_, theSum) => theSum + 1})
  }
  println(s"myLength: ${myLength(List("a","","",""))}")

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case a :: ass => foldLeft(ass, f(z, a))(f)
    }
  }
  def myFlatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, List[B]())({(b, a) =>
      b ++ f(a)
    })
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val myResult = foldLeft(sup, (sup.nonEmpty, sub))({ (tValRemaining, a) =>
      val subResult = tValRemaining._2 match {
        case Nil => (tValRemaining._1, Nil)
        case sb :: ssub =>
          (tValRemaining._1 && (sb == a), ssub)
      }
      subResult
    })
    if(myResult._1){
      true
    } else {
      sup match {
        case Nil =>
          val supMatch = false
          supMatch
        case _ :: ssup =>
          hasSubsequence(ssup, sub)
      }
    }
  }
  println(s"${myFlatMap(List(1,2,3))(i => List(i,i))}")
  println(s"Has Subsequence: ${hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5))}")
  val x = List(1,2,3,4,5) match {
    case _x :: 2 :: 4 :: _ => _x
    case Nil => 42
    case _x :: y :: 3 :: 4 :: _ => _x + y
    case h :: t => h + t.sum
    case _ => 101
  }

  sealed trait MyTree[+A]
  case class MyLeaf[A](value: A) extends MyTree[A]
  case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

  def myTreeSize[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => myTreeSize(l) + myTreeSize(r)
  }


}
