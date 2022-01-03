package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CandyDispenserSpec extends AnyFlatSpec with Matchers {
  private val dispenser = CandyDispenser(candies = 10)

  "machine" should "be locked with zero coins, and a number of candies" in {
    dispenser.candies shouldBe 10
    dispenser.coins shouldBe 0
    dispenser.locked shouldBe true
  }

  it should "be unlocked given a coin" in {
    val state = dispenser.simulate(List(Coin))

    state.run(dispenser)._2.locked shouldBe false
  }

  it should "have one more coin given a coin" in {

  }
}
