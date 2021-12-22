package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EitherSpec extends AnyFlatSpec with Matchers {
  "map (ex 4.6)" should "operate on right given value" in {
    Right("boom").map((s: String) => s.toUpperCase()) shouldBe Right("BOOM")
  }

  it should "return left given left" in {
    Left("boom").map((s: String) => s.toUpperCase()) shouldBe Left("boom")
  }
}
