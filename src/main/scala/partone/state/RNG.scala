package partone.state

trait RNG {
  def nextInt(): (Int, RNG)
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt()
    (math.abs(n), next)
  }
}
