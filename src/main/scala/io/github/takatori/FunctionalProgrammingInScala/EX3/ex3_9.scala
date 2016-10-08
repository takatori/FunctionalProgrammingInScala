package io.github.takatori.FunctionalProgrammingInScala.EX3

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_9 {
  def main(args: Array[String]): Unit = {
    println(length(List(1,2,3,4,5)))
  }

  def length[A](as: List[A]): Int = List.foldRight(as, 0)((x, y) => y + 1)
}
