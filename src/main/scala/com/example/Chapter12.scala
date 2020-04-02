package com.example

import Chapter11._
object Chapter12 {
  trait Applicative[F[_]] {
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]
    def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      fas.foldLeft(unit(List[A]()))({case (result, elem) =>
        map2(result, elem)({(a,b)=>
          a ++ List(b)
        })
      })
    }
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
      sequence(List.fill(n)(fa))
    }
    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
      map2(fa, fb)({(a,b) =>
        (a, b)
      })
    }
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
      map2(fab, fa)({(f, a) =>
        f(a)
      })
    }
    def otherMap[A,B](fa: F[A])(f: A => B): F[B] = {
      apply(unit(f))(fa)
    }
    def otherMap2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
      apply(apply(unit(f.curried))(fa))(fb)
    }
    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
    }
    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = {
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
    }
  }
  def eitherMonad[E]: Mon[({type f[x] = Either[E, x]})#f] = new Mon[({type f[x] = Either[E, x]})#f]{
    override def unit[A](a: => A): Either[E, A] = {
      Right(a)
    }
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      fa match {
        case Left(l) => Left(l)
        case Right(r) => f(r)
      }
    }
  }
  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      fa match {
        case Success(a) =>
          fb match {
            case Success(b) =>
              Success(f(a,b))
            case Failure(h,t) => Failure(h,t)
          }
        case Failure(h,t) =>
          fb match {
            case Success(_) => Failure(h,t)
            case Failure(hh,tt) => Failure(h, Vector(hh) ++ tt ++ t)
          }
      }
    }

    override def unit[A](a: => A): Validation[E, A] = {
      Success(a)
    }
  }
}
