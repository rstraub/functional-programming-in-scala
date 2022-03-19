package parttwo.parallelism

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParTest extends AnyWordSpec with Matchers {
  "asyncF" should {
    "evaluate result asynchronously" in {
      val fun = (i: Int) => {
        val res = i.toString
        println()
        res
      }

      val result = Par.asyncF(fun)(1)

      result shouldBe "1"
    }
  }
}
