package com.example

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