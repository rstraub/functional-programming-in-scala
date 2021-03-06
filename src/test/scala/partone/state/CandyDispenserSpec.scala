package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.state.CandyDispenser.simulate

class CandyDispenserSpec extends AnyFlatSpec with Matchers {
  private val dispenser = CandyDispenser(candies = 10)

  "machine" should "be locked with zero coins, and a number of candies" in {
    dispenser.candies shouldBe 10
    dispenser.coins shouldBe 0
    dispenser.locked shouldBe true
  }

  it should "be unlocked given a coin on locked machine" in {
    val result = simulateInputsOnDispenser(Coin, Turn, Coin)

    result._2.locked shouldBe false
  }

  it should "have one more coin given a coin on locked machine" in {
    val result = simulateInputsOnDispenser(Coin, Turn, Coin)

    result._2.coins shouldBe dispenser.coins + 2
  }

  it should "dispense candy given turn on unlocked machine" in {
    val result = simulateInputsOnDispenser(Coin, Turn, Coin, Turn)

    result._2.candies shouldBe dispenser.candies - 2
  }

  it should "lock after turning" in {
    val result = simulateInputsOnDispenser(Coin, Turn, Coin, Turn)
    result._2.locked shouldBe true
  }

  private def simulateInputsOnDispenser(
      inputs: Input*
  ): ((Int, Int), CandyDispenser) = {
    val state = simulate(inputs.toList)

    state.run(dispenser)
  }
}
