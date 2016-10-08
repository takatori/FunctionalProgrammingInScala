package io.github.takatori.FunctionalProgrammingInScala.EX2

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex2_4 {
  def uncurry[A,B,C](f: B => C, g: A => B): (A,B) => C = (a: A, b: B) => f(g(a), b)
}
