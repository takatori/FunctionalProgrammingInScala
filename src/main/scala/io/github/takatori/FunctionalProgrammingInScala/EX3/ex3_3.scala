package io.github.takatori.FunctionalProgrammingInScala.EX3

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_3 {

  def main(args:Array[String]): Unit = {
    println(setHead(List(1,2,3,4,5), 10))
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }
}
