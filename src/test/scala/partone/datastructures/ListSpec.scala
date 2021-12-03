package partone.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.datastructures.List.{setHead, sum, tail}


class ListSpec extends AnyFlatSpec with Matchers {
  "list" should "be Nil given no arguments" in {
    val result = List()

    result shouldBe Nil
  }

  it should "be Cons given variadic arguments" in {
    val result = sum(List(1, 2))

    result shouldBe 3
  }

  "pattern match" should "be what I think it should be (ex 3.1)" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x shouldBe 3
  }

  "tail" should "remove the first element of a list (ex 3.2)" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "return Nil given Nil" in {
    tail(Nil) shouldBe Nil
  }

  "setHead" should "add a new element to the start of the list (ex 3.3)" in {
    setHead(List(2, 3), 1) shouldBe List(1, 2, 3)
  }

  it should "return list of one element given Nil" in {
    setHead(Nil, "test") shouldBe List("test")
  }
}
