package partone.state

import partone.state.State.{modify, sequence}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class CandyDispenser(locked: Boolean = true, coins: Int = 0, candies: Int) {
  private def update(input: Input): CandyDispenser =
    (input, this) match {
      case (Turn, CandyDispenser(false, _, _)) => CandyDispenser(true, 1, 9)
      case _ => CandyDispenser(false, 1, 9)
    }
}

object CandyDispenser {
  def simulate(inputs: List[Input]): State[CandyDispenser, (Int, Int)] = for {
    _ <- sequence(inputs map (i => modify[CandyDispenser](_ update i)))
    s <- State.get
  } yield (s.coins, s.candies)
}
