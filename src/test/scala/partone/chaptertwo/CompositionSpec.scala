package partone.chaptertwo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.chaptertwo.Composition.compose

class CompositionSpec extends AnyFlatSpec with Matchers {
  "compose" should "pipe the output of the first function into the second" in {
    val numToString = (n: Int) => n.toString
    val withDonuts = (s: String) => s + " Donuts"

    val composed = compose(numToString, withDonuts)

    composed(3) shouldBe "3 Donuts"
  }
}
