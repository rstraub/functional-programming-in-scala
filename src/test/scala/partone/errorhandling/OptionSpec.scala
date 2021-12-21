package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionSpec extends AnyFlatSpec with Matchers {
  "map (ex 4.1)" should "operate on value if present" in {
    Some(1).map(_ * 2) shouldBe Some(2)
  }
}
