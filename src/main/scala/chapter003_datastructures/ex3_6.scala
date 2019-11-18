package chapter003_datastructures

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_6 {
  def main(args: Array[String]): Unit = {
    println(init(List(1,2,3,4,5)))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}
