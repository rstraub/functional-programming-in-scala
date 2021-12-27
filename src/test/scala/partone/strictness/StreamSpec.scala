package partone.strictness

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamSpec extends AnyFlatSpec with Matchers {
  "apply" should "create an empty, typed stream given no elements" in {
    Stream[Int]() shouldBe Empty
  }

  it should "create stream given elements" in {
    Stream(1, 2).isInstanceOf[Cons[Int]] shouldBe true
  }

  "toList" should "convert to an empty list given empty stream" in {
    Stream().toList shouldBe List()
  }

  it should "convert to list given non-empty stream" in {
    Stream(1, 2).toList shouldBe List(1, 2)
  }

  "take (ex 5.2)" should "return first n elements" in {
    Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
  }

  it should "return elements on depleted stream" in {
    Stream(1, 2).take(3).toList shouldBe List(1, 2)
  }

  it should "return empty on empty stream" in {
    Stream().take(3).toList shouldBe List()
  }

  "drop" should "return list without first n elements" in {
    Stream(1, 2, 3).drop(2).toList shouldBe List(3)
  }

  it should "return empty given empty stream" in {
    Stream().drop(1) shouldBe Empty
  }

  it should "return empty given depleted stream" in {
    Stream(1, 2).drop(3) shouldBe Empty
  }

  "takeWhile (ex 5.3)" should "return starting elements matching predicate" in {
    Stream(1, 2, 3, 10, 5).takeWhile(_ < 10).toList shouldBe List(1, 2, 3)
  }

  it should "return empty given elements don't match predicate" in {
    Stream(1, 2, 3).takeWhile(_ > 3).toList shouldBe List()
  }
}