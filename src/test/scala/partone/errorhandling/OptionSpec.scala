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

  "flatMap" should "return a new option given function" in {
    Some(1).flatMap(a => if (a == 1) Some(a) else None) shouldBe Some(1)
  }

  "getOrElse" should "return value given some" in {
    Some(2).getOrElse(1) shouldBe 2
  }

  it should "return default otherwise" in {
    None.getOrElse(1) shouldBe 1
  }

  private def multiplyByTwo(a: Int) = a * 2
}
