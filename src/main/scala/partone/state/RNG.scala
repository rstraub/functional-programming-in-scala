package partone.state

trait RNG {
  def nextInt(): (Int, RNG)
}

object RNG {
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, rng2) = nonNegativeInt(rng)
    val n = num / (Int.MaxValue.toDouble + 1)
    (n, rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt()
    val positiveInt = if (n == Int.MinValue) Int.MaxValue else math.abs(n)
    (positiveInt, next)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt()
    val (d, r2) = double(r)
    ((i, d), r2)
  }
}
