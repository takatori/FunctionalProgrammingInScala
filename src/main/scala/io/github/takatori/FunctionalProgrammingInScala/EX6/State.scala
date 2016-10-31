package io.github.takatori.FunctionalProgrammingInScala.EX6


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRNG)
    }
  }

  // typeを使って型（クラス・トレイト・関数型等）に別名（alias）を付けられる。
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // EX6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
      rng.nextInt match {
        case (Int.MinValue, r) => (0, r)
        case _ => _
    }
  }
}