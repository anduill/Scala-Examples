package com.example

import shapeless.Generic
import shapeless.{::, HList, HNil, Lazy}
import shapeless._

trait CsvEncoder[A] {
  def encode(value:A): List[String]
}
object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc
  def instance[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      override def encode(value: A): List[String] = func(value)
    }
}
case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

/**
implicit val booleanEncoder: CsvEncoder[Boolean] = CsvEncoder.instance(b => if(b) List("yes") else List("no"))
    implicit val doubleEncoder: CsvEncoder[Double] = CsvEncoder.instance(d => List(d.toString))
    implicit val intEncoder: CsvEncoder[Int] = CsvEncoder.instance(i => List(i.toString))
    implicit val stringEncoder: CsvEncoder[String] = CsvEncoder.instance(s => List(s))
    implicit val hnilEncoder: CsvEncoder[HNil] = CsvEncoder.instance(hnil => Nil)
    implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = CsvEncoder.instance {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = values.map(value => enc.encode(value).mkString(",")).mkString("\n")
    /**
  * We can use the encoders we have defined above to construct specific encoders
  */
//    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
//      val gen = Generic[IceCream]
//      val enc = CsvEncoder[gen.Repr]
//      CsvEncoder.instance {iceCream => enc.encode(gen.to(iceCream))}
//    }

    /**
  * let's make a more generic encoder
  */
//    implicit val gen = Generic[IceCream]// no longer needed
    implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A,R], enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] = CsvEncoder.instance {a => enc.value.encode(gen.to(a))}

    implicit def listEncoder[A](implicit enc: CsvEncoder[A]): CsvEncoder[List[A]] = CsvEncoder.instance({ l =>
      l.foldLeft(List[String]()) {(res, el) => res ++ enc.encode(el)}
    })

    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
    val reprExample = reprEncoder.encode("pistachio" :: 2 :: true :: HNil)


    val outString = reprExample.toString()
    val iceCreamEx = reprEncoder.encode(Generic[IceCream].to(IceCream("pistachio",2,true)))
    val iceCream1 = IceCream("pistachio",2,true)
    val iceCream2 = IceCream("vanilla",1,true)
    val iceCreams = List(iceCream1, iceCream2)

    println(reprExample)
    println(iceCreamEx)
    println(listEncoder[IceCream].encode(iceCreams))


    import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
    implicit val cnilEncoder: CsvEncoder[CNil] = CsvEncoder.instance(cnil => throw new Exception("Inconceivable!"))
    implicit def coproductEncoder[H, T <: Coproduct](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = CsvEncoder.instance {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    val shapes: List[Shape] = List(
      Rectangle(3.0, 4.0),
      Circle(1.0)
    )
    val rectangles = List(Rectangle(3.0, 4.0),Rectangle(1.0, 2.0),Rectangle(5.0, 6.0))
    println(writeCsv(shapes))


    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]



    //Simple Type Refinement exercise
    sealed abstract class MList {self =>
      type T
      def uncons: Option[MCons {type T = self.T}]
    }
    sealed abstract class MNil extends MList {
      def uncons = None
    }
    sealed abstract class MCons extends MList {self =>
      val head: T
      val tail: MList {type T = self.T}
      def uncons = Some(self: MCons {type T = self.T})
    }


    def MNil[T0](): MNil {type T = T0} =
      new MNil {
        type T = T0
      }
    def MCons[T0](hd: T0, tl: MList {type T = T0})
    : MCons {type T = T0} =
      new MCons {
        type T = T0
        val head = hd
        val tail = tl
      }
    val nums = MCons(2, MCons(3, MNil())): MCons{type T = Int}
    val res3 = nums.tail.uncons.map(_.head)
    val res1 = nums.head
    val res2 = res1 + res1
    val res4 = res3.map(_ - res2)
    println(res4)
  }

  **/