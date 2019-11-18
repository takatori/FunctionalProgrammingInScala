package chapter004_errorhandling

/**
  * Created by takatorisatoshi on 2016/10/09.
  */
object ex4_4 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):Option[C] = a.flatMap(sa => b.map(sb => f(sa,sb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as.foldRight(Some(Nil): Option[List[A]])((a: Option[A], acc: Option[List[A]]) => {
    map2(a, acc)((x, xs) => x :: xs)
  })

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
}
