package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.errorhandling.Either.{Try, sequence, traverse}

class EitherSpec extends AnyFlatSpec with Matchers {
  "map (ex 4.6)" should "operate on right given value" in {
    Right("boom").map((s: String) => s.toUpperCase()) shouldBe Right("BOOM")
  }

  it should "return left given left" in {
    Left("boom").map((s: String) => s.toUpperCase()) shouldBe Left("boom")
  }

  "flatMap" should "return new right given successful" in {
    Right("1").flatMap(tryParse) shouldBe Right(1)
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

  "sequence (ex 4.7)" should "return right of list given all succeed" in {
    sequence(List(tryParse("1"))) shouldBe Right(List(1))
  }

  it should "return left of list given any fails" in {
    sequence(List(tryParse(""))).isInstanceOf[Left[NumberFormatException]] shouldBe true
  }

  "traverse" should "return right of list given all transforms succeed" in {
    traverse(List("1"))(tryParse) shouldBe Right(List(1))
  }

  it should "return left given any transform fails" in {
    traverse(List(""))(tryParse).isInstanceOf[Left[NumberFormatException]] shouldBe true
  }

  private def tryParse(s: String) = Try {
    s.toInt
  }
}
