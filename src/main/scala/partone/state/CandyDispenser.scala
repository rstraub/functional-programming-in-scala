package partone.state

import partone.state.State.{get, modify}

sealed trait Input

case object Coin extends Input

case class CandyDispenser(locked: Boolean = true, coins: Int = 0, candies: Int) {
  def simulate(inputs: List[Input]): State[CandyDispenser, (Int, Int)] = for {
    _ <- modify[CandyDispenser](_ => update(inputs.head))
    s <- get
  } yield (s.coins, s.candies)

  private def update(input: Input): CandyDispenser = {
    CandyDispenser(false, coins, candies)
  }
}
