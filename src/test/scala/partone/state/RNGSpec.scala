package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RNGSpec extends AnyFlatSpec with Matchers {
  "nonNegativeInt (ex 6.1)" should "return random positive int given positive number" in {
    RNG.nonNegativeInt(SimpleRNG(42))._1 shouldBe 16159453
  }

  "nonNegativeInt (ex 6.1)" should "return random positive int given negative number" in {
    RNG.nonNegativeInt(SimpleRNG(-42))._1 shouldBe 16159454
  }

  it should "return positive int given min value for int" in {
    object Fake extends RNG {
      override def nextInt(): (Int, RNG) = (Int.MinValue, Fake)
    }

    RNG.nonNegativeInt(Fake)._1 shouldBe Int.MaxValue
  }

  "double (ex 6.2)" should "return random double given rng" in {
    RNG.double(SimpleRNG(42))._1 shouldBe 0.007524831686168909
  }
}
