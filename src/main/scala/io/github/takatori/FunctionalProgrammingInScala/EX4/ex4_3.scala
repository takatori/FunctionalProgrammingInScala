package io.github.takatori.FunctionalProgrammingInScala.EX4

/**
  * Created by takatorisatoshi on 2016/10/09.
  */
object ex4_3 {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):Option[C] = a.flatMap(sa => b.map(sb => f(sa,sb)))

}
