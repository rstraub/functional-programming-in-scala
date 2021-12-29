package partone.strictness

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.strictness.Stream.cons

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

  "forAll" should "return true given all elements match predicate" in {
    Stream(1, 2, 3).forAll(_ <= 3) shouldBe true
  }

  it should "return false given an element doesn't match predicate" in {
    Stream(1, 2, 3, 4).forAll(_ < 4) shouldBe false
  }

  it should "terminate early given non-matching element" in {
    cons(1, sys.error("boom!")).forAll(_ > 1) shouldBe false
  }

  "takeWhile via foldRight (ex 5.5)" should "take starting elements matching predicate" in {
    Stream(1, 2, 3, 2).takeWhileZ(_ < 3).toList shouldBe List(1, 2)
  }

  "headOption via foldRight (ex 5.6)" should "return some of first element given non-empty stream" in {
    Stream(1, 2).headOption() shouldBe Some(1)
  }

  it should "return non given empty stream" in {
    Stream.empty().headOption() shouldBe None
  }

  "map (ex 5.7)" should "transform each item in stream" in {
    Stream(1, 2, 3).map(_ * 2).toList shouldBe List(2, 4, 6)
  }

  "filter" should "remove elements not matching predicate" in {
    Stream(1, 2, 3).filter(_ % 2 == 0).toList shouldBe List(2)
  }

  "append" should "add an element to the stream (lazily)" in {
    Stream(1, 2).append(Stream(3)).toList shouldBe List(1, 2, 3)
  }

  "flatMap" should "return single stream given function of streams" in {
    Stream(1, 2, 3).flatMap(a => Stream(a.toString)).toList shouldBe List("1", "2", "3")
  }

  private val ones: Stream[Int] = Stream.cons(1, ones)

  "infinite stream of ones" should "take only what is required" in {
    ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  it should "stop when anything matching is found" in {
    ones.exists(_ % 2 != 0) shouldBe true
  }

  it should "keep trying if it doesnt" in {
    assertThrows[StackOverflowError] {
      ones.exists(_ % 2 == 0)
    }
  }

  // Results in program loop
  it should "keep trying as long as condition matches" ignore {
    ones.takeWhile(_ == 1).toList
  }

  it should "trying forAll until it doesnt match" in {
    ones.forAll(_ != 1) shouldBe false
  }

  "constant (ex 5.8)" should "generate infinite stream for constant" in {
    Stream.constant(1).take(2).toList shouldBe ones.take(2).toList
  }

  "from (ex 5.9)" should "generate infinite stream of integers from n" in {
    Stream.from(2).take(3).toList shouldBe List(2, 3, 4)
  }

  "fibs (ex 5.10)" should "generate fibonacci numbers" in {
    Stream.fibs().takeWhile(_ < 10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "unfold (ex 5.11)" should "generate stream" in {
    Stream.unfold(1)(s => if (s <= 3) Some((s, s + 1)) else None).toList shouldBe List(1, 2, 3)
  }

  "fibsViaUnfold (ex 5.12)" should "generate fibonacci numbers" in {
    Stream.fibsViaUnfold().takeWhile(_ < 10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "fromViaUnfold" should "generate stream of ints" in {
    Stream.fromViaUnfold(2).take(3).toList shouldBe List(2, 3, 4)
  }
}
