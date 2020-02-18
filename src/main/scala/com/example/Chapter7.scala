package com.example

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Chapter7 extends App{
  type Par[A] = ExecutorService => Future[A]

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  object Par{
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {es =>
      val aResult = a(es)
      val bResult = b(es)
      UnitFuture(f(aResult.get, bResult.get))
    }
    def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
    def asyncF[A,B](f: A => B): A => Par[B] = {a =>
      lazyUnit(f(a))
    }
    def fork[A](a: => Par[A]): Par[A] = es => {
      es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
    }
    def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      val (l,r) = ps.splitAt(ps.length/2)
      Par.map2(Par.fork(sequence(l)), Par.fork(sequence(r)))(_ ++ _)
    }
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      as match {
        case Nil => lazyUnit(List())
        case head :: Nil => if(f(head)) lazyUnit(List(head)) else lazyUnit(List())
        case otherList =>
          val (l,r) = otherList.splitAt(as.length/2)
          Par.map2(Par.fork(parFilter(l)(f)), Par.fork(parFilter(r)(f)))({(a, aa) =>
            a ++ aa
          })
      }
    }
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      flatMap(n)({ index => choices(index)})(es)
    }
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => {
      flatMap(cond)({ bool => if(bool) t else f})(es)
    }
    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => {
      choices(key(es).get())(es)
    }
    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = es => {
      join(map(a)(f))(es)
    }
    def join[A](a: Par[Par[A]]): Par[A] = es => {
      map(a)({parA => parA(es).get()})(es)
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false }
}
