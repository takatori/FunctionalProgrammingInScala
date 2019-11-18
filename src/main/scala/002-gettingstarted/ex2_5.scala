package io.github.takatori.FunctionalProgrammingInScala.EX2

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex2_5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
