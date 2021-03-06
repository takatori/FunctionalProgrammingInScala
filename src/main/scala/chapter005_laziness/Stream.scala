package chapter005_laziness

import Stream._

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

  // EX5.6
  def headOption: Option[A] = {
    foldRight(None: Option[A])((h: A, None: Option[A]) => if (h == Empty) None else Some(h))
  }

  // EX5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  // EX5.7
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) t else cons(h, t))

  // EX5.7
  def append(a: => A): Stream[A] =
    foldRight(empty[A])((h, t) => if (h == Empty) cons(a, Empty) else cons(h, t))

  // EX5.7A
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => cons(h,t))

  // EX5.7
  def flatMap[B](f: A => Option[B]): Stream[Option[B]] =
    foldRight(empty[Option[B]])((h, t) => cons(f(h), t))

  // EX5.7A
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  // EX5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // EX5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // EX5.10
  def fibs(a: Int, b:Int): Stream[Int] = Stream.cons(a, fibs(b, a+b))

  // EX5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some(x) => cons(x._1, unfold(x._2)(f))
  }

  // EX5.12
  def fibs2(a: Int, b: Int): Stream[Int] = unfold(a)(a => Some(b, a+b))

  // EX5.12
  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  // EX5.12
  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  // EX5.12
  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

  // EX5.13
  def mapVerUnfold[A, B](as: Stream[A])(f: A => B): Stream[B] =
  unfold(as)(as => as match {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  })

  // EX5.13
  def takeVerUnfold[A](n: Int, stream: Stream[A]): Stream[A] = unfold(stream)(s => s match {
    case Cons(h, t) if n > 0 => Some(h(), takeVerUnfold(n-1, t()))
    case _ => None
  })

  // EX5.13
  def takeWhileVerUnfold[A](f: A => Boolean, stream: Stream[A]): Stream[A] = unfold(stream)(s => s match {
    case Cons(h, t) if f(h()) => Some(h(), takeWhileVerUnfold(f, t()))
    case _ => None
  })
}
