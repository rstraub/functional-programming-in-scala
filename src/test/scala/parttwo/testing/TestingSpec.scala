package parttwo.testing

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestingSpec extends AnyWordSpec with Matchers {
  "&&" should {
    "return a new prop" in {

    }
  }

  "choose" should {
    "generate random numbers between start to end" in {
      val result = Gen.choose(1, 10)
      result.next should be >= 1
      result.next should be <= 10
    }
  }
}
