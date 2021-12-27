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
}
