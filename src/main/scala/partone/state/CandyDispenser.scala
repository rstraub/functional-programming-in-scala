package partone.state

import partone.state.State.{get, modify}

sealed trait Input

case object Coin extends Input

case class CandyDispenser(locked: Boolean = true, coins: Int = 0, candies: Int) {
  def simulate(inputs: List[Input]): State[CandyDispenser, (Int, Int)] = for {
    _ <- modify[CandyDispenser](_ => CandyDispenser(locked, coins, candies))
    s <- get
  } yield (s.coins, s.candies)
}
