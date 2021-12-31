package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RNGSpec extends AnyFlatSpec with Matchers {
  private val fortyTwo: SimpleRNG = SimpleRNG(42)

  private object FakeOne extends RNG {
    override def nextInt(): (Int, RNG) = (1, FakeOne)
  }

  "nonNegativeInt (ex 6.1)" should "return random positive int given positive number" in {
    RNG.nonNegativeInt(fortyTwo)._1 shouldBe 16159453
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
    RNG.double(FakeOne)._1 shouldBe 4.6566128730773926E-10
  }

  "intDouble (ex 6.3)" should "return random int and double" in {
    RNG.intDouble(FakeOne)._1 shouldBe(1, 4.6566128730773926E-10)
  }

  "doubleInt" should "return random double and int" in {
    RNG.doubleInt(FakeOne)._1 shouldBe(4.6566128730773926E-10, 1)
  }

  "double3" should "return three random doubles" in {
    RNG.double3(fortyTwo)._1 shouldBe(0.007524831686168909, 0.5967354853637516, 0.15846728440374136)
  }
}
