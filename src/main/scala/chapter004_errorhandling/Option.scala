package chapter004_errorhandling

/**
  * Created by takatorisatoshi on 2016/10/08.
  */

// EX4.1
sealed trait Option[+A] {
  // OptionがNoneでない場合はfを適用
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // OptionがNoneでない場合は、失敗する可能性のあるfを適用
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // B >: AはパラメータBの型がAのスーパークラスでなければならないことを示す
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap((a: A) => {
    if(f(a)) Some(a)
    else None
  })
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
