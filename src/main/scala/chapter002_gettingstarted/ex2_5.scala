package chapter002_gettingstarted

object ex2_5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
