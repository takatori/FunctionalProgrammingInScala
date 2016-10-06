package io.github.takatori.FunctionalProgrammingInScala.EX2

object ex2_1 {
  def main(args: Array[String]): Unit = println(fib(30))

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }
}