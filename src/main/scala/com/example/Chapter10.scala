package com.example

object Chapter10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty){
      m.zero
    }
    if(v.size == 1){
      v.map(f).foldLeft(m.zero)(m.op)
    } else {
      val groupSize = v.size / 2
      v.grouped(groupSize).foldLeft(m.zero)({(res, vv) =>
        IndexedSeq(res, foldMapV(vv, m)(f)).foldLeft(m.zero)(m.op)
      })
    }
  }
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = {
        (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
      }
      override def zero: (A, B) = {
        (a.zero, b.zero)
      }
    }
  }
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }

  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = {a =>
        b.op(a1(a), a2(a))
      }

      override def zero: A => B = {_ =>
        b.zero
      }
    }
  }
  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) => acc.updated(k, V.op(a.getOrElse(k, V.zero),
        b.getOrElse(k, V.zero)))
      }
  }

  def wcMonoid[A]: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMap(as.toList, wcMonoid[A])({a: A => Map(a -> 1)})
  }

}
