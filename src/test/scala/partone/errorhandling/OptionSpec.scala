package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionSpec extends AnyFlatSpec with Matchers {
  "map (ex 4.1)" should "operate on value if present" in {
    Some(1).map(multiplyByTwo) shouldBe Some(2)
  }

  it should "do nothing on none" in {
    None.map(multiplyByTwo) shouldBe None
  }

  "filter" should "return some if predicate is matched" in {
    Some(1).filter(a => a == 1) shouldBe Some(1)
  }

  it should "return none if predicate is not matched" in {
    Some(1).filter(a => a > 1) shouldBe None
  }

  private def multiplyByTwo(a: Int) = a * 2
}
