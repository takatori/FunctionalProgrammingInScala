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

  // EX6.1 answer
  def nonNegativeIntA(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // EX6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeIntA(rng)
    ((i / Int.MaxValue).toDouble, r)
  }

  // EX6.2 answer
  def doubleA(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
}