package chapter004_errorhandling

/**
  * Created by takatorisatoshi on 2016/10/11.
  */
object ex4_5 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):Option[C] = a.flatMap(sa => b.map(sb => f(sa,sb)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map2(f(x),y)(_ :: _))
}
