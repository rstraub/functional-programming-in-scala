package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.errorhandling.Either.Try

class EitherSpec extends AnyFlatSpec with Matchers {
  "map (ex 4.6)" should "operate on right given value" in {
    Right("boom").map((s: String) => s.toUpperCase()) shouldBe Right("BOOM")
  }

  it should "return left given left" in {
    Left("boom").map((s: String) => s.toUpperCase()) shouldBe Left("boom")
  }

  "flatMap" should "return new right given successful" in {
    Right("1").flatMap(s => Try(s.toInt)) shouldBe Right(1)
  }

  it should "return new left given error" in {
    Right("").flatMap(tryParse).isInstanceOf[Left[NumberFormatException]] shouldBe true
  }

  private def tryParse(s: String) = Try(s.toInt)
}
