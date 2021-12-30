package partone.state

trait RNG {
  def nextInt(): (Int, RNG)
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt()
    val positiveInt = if (n == Int.MinValue) Int.MaxValue else math.abs(n)
    (positiveInt, next)
  }
}
