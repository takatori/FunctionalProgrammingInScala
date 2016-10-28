package io.github.takatori.FunctionalProgrammingInScala.EX6


trait RNG {
  def nextInt: (Int, RNG)
  type Rand[+A] = RNG => (A, RNG)
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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
}