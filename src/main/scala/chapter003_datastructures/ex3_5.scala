package chapter003_datastructures

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_5 {
  def main(args: Array[String]): Unit = {
    println(dropWhile(List(1,2,3,4,5), (i: Int) => i < 3))
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case Nil => Nil
      case _ => l
    }
  }
}
