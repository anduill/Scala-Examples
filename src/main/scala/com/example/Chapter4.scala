package com.example

import java.lang.IllegalArgumentException

object Chapter4 {

  sealed trait MyOption[+A] {
    def map[B](f: A => B): MyOption[B]
    def flatMap[B](f: A => MyOption[B]): MyOption[B]
    def getOrElse[B >: A](default: => B): B
    def getUnSafe(): A
    def orElse[B >: A](ob: => MyOption[B]): MyOption[B]
    def filter(f: A => Boolean): MyOption[A]
  }
  case class MySome[+A](get: A) extends MyOption[A] {
    override def map[B](f: A => B): MyOption[B] = {
      MySome(f(get))
    }

    override def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
      f(get)
    }

    override def getOrElse[B >: A](default: => B): B = {
      get
    }

    override def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
      this
    }

    override def filter(f: A => Boolean): MyOption[A] = {
      if (f(get)) {
        this
      } else {
        MyNone
      }
    }

    override def getUnSafe(): A = get
  }
  case object MyNone extends MyOption[Nothing] {
    override def map[B](f: Nothing => B): MyOption[B] = {
      this
    }

    override def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = {
      this
    }

    override def getOrElse[B >: Nothing](default: => B): B = {
      default
    }

    override def orElse[B >: Nothing](ob: => MyOption[B]): MyOption[B] = {
      ob
    }

    override def filter(f: Nothing => Boolean): MyOption[Nothing] = {
      this
    }

    override def getUnSafe(): Nothing = throw new IllegalArgumentException(s"None Type does not have a Value")
  }

}
