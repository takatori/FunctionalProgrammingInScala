package chapter002_gettingstarted

object ex2_2 {
  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y))
    println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def check(arr: List[A]): Boolean = arr match {
      case x :: Nil => true
      case x :: xs if ordered(x, xs.head) => check(xs)
      case _ => false
    }

    check(as.toList)
  }
}
