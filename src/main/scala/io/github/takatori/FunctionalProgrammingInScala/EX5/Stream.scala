package io.github.takatori.FunctionalProgrammingInScala.EX5
import Stream._
import io.github.takatori.FunctionalProgrammingInScala.EX5

trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // EX 5.1
  def toList: List[A] = {
    def to(stream: Stream[A]): List[A] = stream match {
      case Empty => Nil
      case Cons(h, t) => h() :: to(t())
    }
    to(this)
  }

  // EX 5.2
  def take(n: Int): List[A] = {

    def take2(stream: Stream[A], x: Int): List[A] = stream match {
      case Empty => Nil
      case Cons(h, t) if(x==0) => Nil
      case Cons(h, t) => h() :: take2(t(), x-1)
    }

    take2(this, n)
  }

  // EX5.2
  def drop(n: Int): List[A] = {

    def drop2(stream: Stream[A], x: Int): Stream[A] = stream match {
      case Empty => Empty
      case Cons(h, t) if(x==0) => t()
      case Cons(h, t) => drop2(t(), x-1)
    }
    drop2(this, n)
  }

  // EX5.2 answer
  def takeA(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // EX5.2 answer
  def dropA(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().dropA(n - 1)
    case _ => this
  }

  // EX5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case Cons(h, _) => cons(h(), Empty)
    }
  }

  // EX5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Cons(h, empty) if p(h()) => true
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // EX5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
