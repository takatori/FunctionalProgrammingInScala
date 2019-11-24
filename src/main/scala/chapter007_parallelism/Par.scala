package chapter007_parallelism

import scala.concurrent.duration.TimeUnit


object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
   * 評価されていないAを受け取り、それを別スレッドで評価するための計算を返す。
   * unitという名前は、一つの値をラッピングするだけという1単位の並列化を実現することを意味する。
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def mapPar[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val  fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldLeft[Par[List[A]]](unit(List()))((l, a) => map2(a, l)(_ :: _))


  /**
   * 特定のParを別の論理スレッドで実行する
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
   * 並列計算から結果の値を受け取る
   */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}



object P {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}