package partone.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.datastructures.List.sum


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
}
