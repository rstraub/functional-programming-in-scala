package partone.errorhandling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.errorhandling.Option.{Try, deviation, lift, map2, mean, sequence, variance}

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

  "orElse" should "return some given some" in {
    Some(1).orElse(Some(2)) shouldBe Some(1)
  }

  it should "return specified some given none" in {
    None.orElse(Some(1)) shouldBe Some(1)
  }

  "mean (ex 4.2)" should "return some double given elements" in {
    mean(List(1.0, 3.0, 5.0)) shouldBe Some(3.0)
  }

  it should "return none given no elements" in {
    mean(List()) shouldBe None
  }

  "variance (ex 4.2)" should "return some variance given elements" in {
    variance(List(1.0, 3.0, 5.0)) shouldBe Some((math.pow(2.0, 2) + 0.0 + math.pow(2.0, 2)) / 3)
  }

  it should "return none given no elements" in {
    variance(List()) shouldBe None
  }

  "lift" should "return function in the context of options" in {
    lift(math.abs)(Some(-2)) shouldBe Some(2)
  }

  "map2 (ex 4.3)" should "return combined option" in {
    map2(Try("1".toInt), Try("2".toInt))(_ + _) shouldBe Some(3)
  }

  it should "return none if last argument is none" in {
    map2(Try("1".toInt), Try("@".toInt))(_ + _) shouldBe None
  }

  it should "return none if first argument is none" in {
    map2(Try("@".toInt), Try("1".toInt))(_ + _) shouldBe None
  }

  "sequence (ex 4.4)" should "return option of list given all succeed" in {
    sequence(List(Some(2), Some(1))) shouldBe Some(List(2, 1))
  }

  it should "return none given one fails" in {
    sequence(List(Some(1), None)) shouldBe None
  }

  private def multiplyByTwo(a: Int) = a * 2
}
