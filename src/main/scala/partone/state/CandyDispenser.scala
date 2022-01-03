package partone.state

import partone.state.State.{modify, sequence}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class CandyDispenser(locked: Boolean = true, coins: Int = 0, candies: Int) {
  private def update(input: Input): CandyDispenser =
    input match {
      case Turn => CandyDispenser(true, 1, candies - 1)
      case _ => CandyDispenser(false, coins + 1, candies)
    }
}

object CandyDispenser {
  def simulate(inputs: List[Input]): State[CandyDispenser, (Int, Int)] = for {
    _ <- sequence(inputs map (i => modify[CandyDispenser](_ update i)))
    s <- State.get
  } yield (s.coins, s.candies)
}
