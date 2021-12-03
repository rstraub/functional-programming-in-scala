package partone.gettingstarted

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.gettingstarted.IsSorted.isSorted

class IsSortedSpec extends AnyFlatSpec with Matchers {
  "is sorted" should "return true for a sorted array" in {
    val sorted = Array(1, 2, 3, 4, 5)

    isSorted(sorted, (x: Int, y: Int) => x < y) shouldBe true
  }

  it should "return false for an unsorted array" in {
    val unsorted = Array(1, 2, 7, 5)

    isSorted(unsorted, (x: Int, y: Int) => x < y) shouldBe false
  }

  it should "return true for any sorted array" in {
    val sorted = Array("al", "alp", "alpa", "alpac", "alpaca")

    isSorted(sorted, (a: String, b: String) => a.length < b.length) shouldBe true
  }

  it should "return false for any unsorted array" in {
    val unsorted = Array("alpaca", "alpac", "alpa", "alp", "al")

    isSorted(unsorted, (a: String, b: String) => a.length < b.length) shouldBe false
  }
}
