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

  "orElse" should "return other right given right" in {
    Right("yes!").orElse(Left("no!")) shouldBe Right("yes!")
  }

  it should "return other either given left" in {
    Left("no!").orElse(Right("yes!")) shouldBe Right("yes!")
  }

  "map2" should "return new right given both succeed" in {
    Right(1).map2(tryParse("2"))(_ + _) shouldBe Right(3)
  }

  it should "return left given first fails" in {
    tryParse("@").map2(Right(1))(_ + _)
  }

  it should "return left given second fails" in {
    Right(1).map2(tryParse("@"))(_ + _)
  }

  private def tryParse(s: String) = Try {
    s.toInt
  }
}
