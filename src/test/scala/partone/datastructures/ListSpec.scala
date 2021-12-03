package partone.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.datastructures.List.{drop, setHead, sum, tail}


class ListSpec extends AnyFlatSpec with Matchers {
  "list" should "be Nil given no arguments" in {
    val result = List()

    result shouldBe Nil
  }

  it should "be Cons given variadic arguments" in {
    val result = sum(List(1, 2))

    result shouldBe 3
  }

  "pattern match (ex 3.1)" should "be what I think it should be" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x shouldBe 3
  }

  "tail (ex 3.2)" should "remove the first element of a list" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "return Nil given Nil" in {
    tail(Nil) shouldBe Nil
  }

  "setHead (ex 3.3)" should "add a new element to the start of the list" in {
    setHead(List(2, 3), 1) shouldBe List(1, 2, 3)
  }

  it should "return list of one element given Nil" in {
    setHead(Nil, "test") shouldBe List("test")
  }

  "drop (ex 3.4)" should "remove the specified amount of elements" in {
    drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  it should "return Nil given no more elements to drop" in {
    drop(List(1), 2) shouldBe Nil
  }

  it should "return Nil given Nil" in {
    drop(Nil, 1) shouldBe Nil
  }
}
