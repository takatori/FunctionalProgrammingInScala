package io.github.takatori.FunctionalProgrammingInScala.EX2

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex2_4 {
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)
}
