package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CandyDispenserSpec extends AnyFlatSpec with Matchers {
  "machine" should "be created with zero coins, unlocked and a number of candies" in {
    val result = CandyDispenser(candies = 10)

    result.candies shouldBe 10
    result.coins shouldBe 0
    result.locked shouldBe false
  }
}
