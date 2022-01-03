package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.state.RNG._

class RNGSpec extends AnyFlatSpec with Matchers {
  private val fortyTwo: SimpleRNG = SimpleRNG(42)

  private object FakeOne extends RNG {
    override def nextInt(): (Int, RNG) = (1, FakeOne)
  }

  "nonNegativeInt (ex 6.1)" should "return random positive int given positive number" in {
    nonNegativeInt(fortyTwo)._1 shouldBe 16159453
  }

  "nonNegativeInt (ex 6.1)" should "return random positive int given negative number" in {
    nonNegativeInt(SimpleRNG(-42))._1 shouldBe 16159454
  }

  it should "return positive int given min value for int" in {
    object Fake extends RNG {
      override def nextInt(): (Int, RNG) = (Int.MinValue, Fake)
    }

    nonNegativeInt(Fake)._1 shouldBe Int.MaxValue
  }

  "double (ex 6.2)" should "return random double given rng" in {
    double(FakeOne)._1 shouldBe 4.6566128730773926E-10
  }

  "intDouble (ex 6.3)" should "return random int and double" in {
    intDouble(FakeOne)._1 shouldBe(1, 4.6566128730773926E-10)
  }

  "doubleInt" should "return random double and int" in {
    doubleInt(FakeOne)._1 shouldBe(4.6566128730773926E-10, 1)
  }

  "double3" should "return three random doubles" in {
    double3(fortyTwo)._1 shouldBe(0.007524831686168909, 0.5967354853637516, 0.15846728440374136)
  }

  "ints (ex 6.4)" should "generate list of random ints" in {
    RNG.ints(3)(fortyTwo)._1 shouldBe List(16159453, -1281479697, -340305902)
  }

  "doubleViaMap (ex 6.5)" should "return random double" in {
    RNG.doubleViaMap()(FakeOne)._1 shouldBe 4.6566128730773926E-10
  }

  "map2 (ex 6.6)" should "join two randoms" in {
    RNG.map2(double, nonNegativeInt)((d, i) => s"$d, $i")(FakeOne)._1 shouldBe "4.6566128730773926E-10, 1"
  }

  "sequence (ex 6.7)" should "combine list of transitions" in {
    RNG.sequence(List(unit(1), unit(2), unit(3)))(FakeOne)._1 shouldBe List(1, 2, 3)
  }

  "intsViaSequence" should "return list of n random ints" in {
    RNG.intsViaSequence(3)(fortyTwo)._1 shouldBe List(16159453, 1281479697, 340305902)
  }

  "nonNegativeLessThan (ex 6.8)" should "generate random num between 0 and n (inclusive)" in {
    RNG.nonNegativeLessThan(10)(fortyTwo)._1 shouldBe 3
  }
}
