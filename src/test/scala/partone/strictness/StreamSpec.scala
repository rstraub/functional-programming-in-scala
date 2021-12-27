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
}
