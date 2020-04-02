package com.example

import cats.Monad
import Chapter6._

object Chapter11 extends App {
  trait Mon[F[_]] {
    def unit[A](a: => A): F[A]
    def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a,b)))
    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      lma.foldLeft(unit(List[A]()))({(res, elem) =>
        map2(elem, res)({(a1, a2) =>
          a1 :: a2
        })
      })
    }
    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
      sequence(la.map(f))
    }
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      sequence(List.fill(n)(ma))
    }
    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
//    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
//      flatMap(sequence(ms.map(f).zip(ms.map(unit)).map({case (a1, a2) =>
//        map2(a1, a2)({(b1, b2) =>
//          (b1, b2)
//        })
//      })))({finalA =>
//        unit(finalA.filter(_._1).map(_._2))
//      })
//    }
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = { a =>
      flatMap(f(a))(g)
    }
    def otherFlatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = {
      compose((_:Unit) => fa, f)(())
    }
    def join[A](mma: F[F[A]]): F[A] = {
      flatMap(mma)({ma => ma})
    }
    def otherCompose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {a =>
      val x = join(unit(f(a)))
      val y = map(x)({b =>
        g(b)
      })
      join(y)
    }
  }
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = {
      flatMap({_ => Id(f(value))})
    }
    def flatMap[B](f: A => Id[B]): Id[B] = {
      f(value)
    }
  }
  val idMonad = new Mon[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = {
      f(fa.value)
    }
  }
  def stateMonad[S] = new Mon[({type f[x] = MyState[S,x]})#f] {
    override def unit[A](a: => A): MyState[S, A] = {
      State.unit(a)
    }
    override def flatMap[A, B](fa: MyState[S, A])(f: A => MyState[S, B]): MyState[S, B] = {
      State.flatMap(fa)(f)
    }
  }

  val strState: MyState[Int, String] = stateMonad.unit("hello")
  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Mon[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A] = {
        Reader({_ => a})
      }
      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = {
        val ff = {r: R =>
          f(st.run(r)).run(r)
        }
        Reader(ff)
      }
    }
  }

  trait MyExtendedMonad[F[_]] extends Monad[F] {
    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      lma.foldLeft(pure(List[A]()))({(res, elem) =>
        map2(elem, res)({(a1, a2) =>
          a1 :: a2
        })
      })
    }
    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
      sequence(la.map(f))
    }
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      sequence(List.fill(n)(ma))
    }
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      flatMap(sequence(ms.map(f).zip(ms.map(pure)).map({case (a1, a2) =>
        map2(a1, a2)({(b1, b2) =>
          (b1, b2)
        })
      })))({finalA =>
        pure(finalA.filter(_._1).map(_._2))
      })
    }
  }
  val myOptionMonad = new MyExtendedMonad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa.flatMap({a =>
        f(a)
      })
    }

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = {
      f(a).flatMap({eab =>
        eab match {
          case Left(_) => None
          case Right(b) => pure(b)
        }
      })
    }

    override def pure[A](x: A): Option[A] = Option(x)
  }
  val myListMonad = new MyExtendedMonad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
      fa.flatMap(f(_))
    }

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      f(a).flatMap({eab =>
        eab match {
          case Left(_) => List()
          case Right(value) => List(value)
        }
      })
    }

    override def pure[A](x: A): List[A] = List(x)
  }
  def strBoolFunc(prefix: String): String => Option[Boolean] = { str =>
    if(str.contains(prefix)){
      if(str.startsWith(prefix)){
        Option(true)
      } else {
        Option(false)
      }
    } else {
      None
    }
  }
  def strBoolList(prefix: String): String => List[Boolean] = {str =>
    if(str.contains(prefix)){
      if(str.startsWith(prefix)){
        List(true, true)
      } else {
        List(true, false)
      }
    } else {
      List(false)
    }
  }
  val testList = List("bear", "debe", "cabe", "zoobe", "bat", "cab", "beer")
  val beFilter = strBoolFunc("be")
  val listBeFilter = strBoolList("be")
  val newList = myListMonad.filterM(testList)(listBeFilter)

  println(s"New List: $newList")
}
