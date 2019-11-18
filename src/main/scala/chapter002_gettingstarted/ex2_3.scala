package chapter002_gettingstarted

object ex2_3 {
  def main(args: Array[String]): Unit = {
    curry((a:Int, b:Int) => a + b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)
}
