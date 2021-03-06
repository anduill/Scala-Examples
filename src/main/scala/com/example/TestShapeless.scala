package com.example

import shapeless.labelled.{KeyTag, FieldType}
import shapeless.labelled.field
import shapeless.Witness
import shapeless.syntax.singleton._
import shapeless.LabelledGeneric


object TestShapeless extends App {
  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value
  def getFieldValue[K, V](value: FieldType[K, V]): V = value
  val cher = "numCherries" ->> 123
  val gar = "cat" ->> "garfield"

  println(getFieldName(cher))
  println(getFieldValue(cher))
  println(getFieldName(gar))
  val myList = List(("name", 2), ("num", 'v'))
  println(myList)

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCream = IceCream("Sundae", 1, false)
  val gen = LabelledGeneric[IceCream].to(iceCream)

}
