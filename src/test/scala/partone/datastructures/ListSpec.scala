package partone.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.datastructures.List._


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

  "dropWhile (ex 3.5)" should "remove elements as long as they match predicate" in {
    val input = List(1, 2, 3, 4)

    val result = dropWhile(input)(_ <= 2)

    result shouldBe List(3, 4)
  }

  "init (ex 3.6)" should "return the list without the last element" in {
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "throw an error given Nil" in {
    an[RuntimeException] should be thrownBy init(Nil)
  }

  "sum2" should "use foldRight to achieve its results" in {
    sum2(List(1, 2, 3)) shouldBe 6
  }

  "product2 (ex 3.7)" should "return product of nums" in {
    product2(List(1, 2, 3)) shouldBe 6
  }

  it should "short circuit if zero is encountered (maybe)" in {
    product2(List(1, 0, 2, 3)) shouldBe 0
  }

  "passing Nil and Cons to foldRight (ex 3.8)" should "say something about data constructors" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  "length using foldRight (ex. 3.9)" should "return list length" in {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  "foldLeft (ex 3.10)" should "be 'stack-safe' by using tail-recursion" in {
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  "sum / product / length (ex 3.11)" should "be solved using foldLeft" in {
    List.sumLeft(List(1, 2, 3)) shouldBe 6
    List.productLeft(List(1, 2, 3)) shouldBe 6.0
    List.lengthLeft(List(1, 2, 3)) shouldBe 3
  }

  "reverse (ex 3.12)" should "return a list with elements in reversed order" in {
    List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  "append (ex 3.14)" should "add list to end of other list" in {
    List.append(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
  }

  "concat (ex 3.15" should "flatten and append list of lists" in {
    List.concat(List(List(1, 2), List(3, 4))) shouldBe List(1, 2, 3, 4)
  }

  "incrementByOne (ex 3.16)" should "add one to a each number in the list" in {
    List.incrementByOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  "doublesToString (ex 3.17)" should "turn all doubles to strings" in {
    List.doublesToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  "map (ex  3.18)" should "modify each element while retaining structure" in {
    List.map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)
  }

  "filter (ex 3.19)" should "remove elements not satisfying predicate" in {
    List.filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "flatMap (ex 3.20)" should "map and flatten result" in {
    List.flatMap(List(1, 2, 3))(a => List(a, a)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "filterFlatmap (ex 3.21)" should "filter using flatmap" in {
    List.filterFlatmap(List(1, 2, 3))(_ >= 2) shouldBe List(2, 3)
  }

  "addPairwise (ex 3.22)" should "add elements of both lists together" in {
    List.addPairwise(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  "zipWith (ex 3.23)" should "zip elements with a function" in {
    List.zipWith(List(1, 2, 3), List(2, 3, 4))(_ + _) shouldBe List(3, 5, 7)
  }
}
