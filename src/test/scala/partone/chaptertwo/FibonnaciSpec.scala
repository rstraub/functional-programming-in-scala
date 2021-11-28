package partone.chaptertwo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.chaptertwo.Fibonnaci.fibonnaci

class FibonnaciSpec extends AnyFlatSpec with Matchers {
  "fibonnaci" should "have 0 as the first number" in {
    fibonnaci(0) shouldBe 0
  }

  it should "have 1 as the second number" in {
    fibonnaci(1) shouldBe 1
  }

  it should "have 1 as the third number" in {
    fibonnaci(2) shouldBe 1
  }

  it should "have 2 as the fourth number" in {
    fibonnaci(3) shouldBe 2
  }

  it should "have 3 as the fifth number" in {
    fibonnaci(4) shouldBe 3
  }

  it should "return the nth number of fibonnaci" in {
    fibonnaci(6) shouldBe 8
  }
}
