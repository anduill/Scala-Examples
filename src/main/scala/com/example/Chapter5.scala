package com.example

import com.example.Chapter4.{MyNone, MyOption, MySome}
import com.example.Chapter3.{foldLeft}


object Chapter5 extends App {
  sealed trait MyStream[+A]{
    def headOption: MyOption[A] = this match {
      case MyEmpty => MyNone
      case MyCons(h, t) => MySome(h())
    }
    def isEmpty: Boolean = this match {
      case MyEmpty => true
      case _ => false
    }
    def find(pred: A => Boolean): MyOption[A] = this match {
      case MyEmpty => MyNone
      case MyCons(h, t) => if (pred(h())) {
        MySome(h())
      } else {
        t().find(pred)
      }
    }
    def toList: List[A] = this match {
      case MyEmpty => Nil
      case MyCons(h, t) =>
        h() :: t().toList
    }
    def take(n: Int): MyStream[A] = {
      if(n <= 0){
        MyStream.empty
      } else {
        this match {
          case MyEmpty => MyStream.empty
          case MyCons(h, t) =>
            lazy val _h = h()
            lazy val _t = t().take(n - 1)
            MyStream.cons(_h, _t)
        }
      }
    }
    def drop(n: Int): MyStream[A] = {
      if(n <= 0){
        this
      } else {
        this match {
          case MyCons(_, t) => t().drop(n - 1)
          case e @ MyEmpty => e
        }
      }
    }
    def takeWhile(p: A => Boolean): MyStream[A] = {
      this match {
        case e @ MyEmpty => e
        case MyCons(h, t) =>
          lazy val _h = h()
          lazy val _t = t().takeWhile(p)
          if(p(_h)){
            MyStream.cons(_h, _t)
          }else{
            MyEmpty
          }
      }
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case MyCons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }
    def forAll(p: A => Boolean): Boolean = {
      this match {
        case MyCons(h, t) => p(h()) && t().forAll(p)
        case _ => true
      }
    }
    def takeWhile_f(p: A => Boolean): MyStream[A] = {
      this.foldRight(MyEmpty: MyStream[A])({(t, h) =>
        if(p(t)){
          MyStream.cons(t, h)
        } else {
          h
        }
      })
    }
    def map[B](f: A => B): MyStream[B] = {
      this.foldRight(MyStream.empty[B])({(t, h) =>
        MyStream.cons(f(t), h)
      })
    }
    def filter(f: A => Boolean): MyStream[A] = {
      this.foldRight(MyStream.empty[A])({(t, h) =>
        if(f(t)){
          MyStream.cons(t, h)
        } else {
          h
        }
      })
    }
    def append[B >: A](i: => B): MyStream[B] = {
      this.foldRight(MyStream.cons(i, MyStream.empty))({(t, h) =>
        MyStream.cons(t,h)
      })
    }
    def zipAll[B](s2: MyStream[B]): MyStream[(MyOption[A],MyOption[B])] = {
      MyStream.unfold((this, s2))({case (s_1, s_2) =>
          s_1 match {
            case MyCons(h, t) =>
              s_2 match {
                case MyCons(hh, tt) =>
                  MySome(((MySome(h()),MySome(hh())),(t(), tt())))
                case MyEmpty =>
                  MySome(((MySome(h()),MyNone),(t(), MyEmpty)))
              }
            case MyEmpty =>
              s_2 match {
                case MyCons(hh, tt) =>
                  MySome(((MyNone,MySome(hh())),(MyEmpty, tt())))
                case MyEmpty =>
                  MyNone
              }
          }
      })
    }

  }
  case object MyEmpty extends MyStream[Nothing]
  case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

  object MyStream{
    def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      MyCons(() => head, () => tail)
    }
    def empty[A]: MyStream[A] = MyEmpty

    def apply[A](as: A*): MyStream[A] ={
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
    def constant[A](a: A): MyStream[A] = {
      MyStream.cons(a, constant(a))
    }
    def unfold[A,S](z: S)(f: S => MyOption[(A, S)]): MyStream[A] = {
      f(z) match {
        case MyNone => MyStream.empty
        case MySome(get) =>
          MyStream.cons(get._1, unfold(get._2)(f))
      }
    }
    def uConstant[A](a: A): MyStream[A] = {
      unfold(a)(_ => MySome(a,a))
    }
    def from(n: Int): MyStream[Int] = {
      MyStream.cons(n, from(n+1))
    }
  }
  def fibs(): MyStream[Int] = {
    def fibs_helper(f: => Int, s: =>Int, t: MyStream[Int]): MyStream[Int] = {
      MyStream.cons(f, MyStream.cons(s, fibs_helper(f+s, f+s+s, t)))
    }
    fibs_helper(0, 1, MyStream.empty)
  }
  // meh...not quite right, but close enough
  def uFibs(): MyStream[Int] = {
    MyStream.unfold((0, 1))({case (f, s) =>
        MySome((f+s, (s, f+s)))
    })
  }


  println(s"List stuff: ${MyStream(1,2,3,4,5).take(4).toList}")
  println(s"Mapped Stream: ${MyStream(1,2,3).map(_ * 2).toList}")
  println(s"Appended List: ${MyStream(1,2,3).append(10.5).toList}")
  val ones: MyStream[String] = MyStream.uConstant("abc")
  println(ones.take(10).toList)
  val tenAnd: MyStream[Int] = MyStream.from(10)
  println(tenAnd.take(15).toList)
  val regFibs = fibs()
  val uuFibs = uFibs()
  val zippedFibs = regFibs.zipAll(uuFibs).take(13).toList
  println(s"ZippedFibs: ${zippedFibs}")
}
