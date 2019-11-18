package chapter003_datastructures

/**
  * Created by takatorisatoshi on 2016/10/08.
  */
object ex3_2 {

  def main(args: Array[String]): Unit = {
    println(tail(List(1,2,3,4,5)))
  }
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
}
