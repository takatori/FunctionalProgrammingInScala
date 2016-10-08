package io.github.takatori.FunctionalProgrammingInScala.EX3

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_4 {

  def main(args:Array[String]): Unit = {
    println(drop(List(1,2,3,4,5), 1))
    println(drop(List(1,2,3,4,5), 4))
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, xs) if n == 0 => l
    case Cons(x, xs) => drop(xs, n-1)
  }
}
