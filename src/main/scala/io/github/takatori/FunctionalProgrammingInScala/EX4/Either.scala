package io.github.takatori.FunctionalProgrammingInScala.EX4


/**
  * Created by takatorisatoshi on 2016/10/12.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this map (f) match {
    case Left(e) => Left(e)
    case Right(b) => b
  }

  def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es foldRight[Either[E, List[A]]] (Right(Nil)) ((x, y) => x.map2(y)(_ :: _))

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

}
