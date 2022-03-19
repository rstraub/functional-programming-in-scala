package parttwo.parallelism

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import parttwo.parallelism.Par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class ParTest extends AnyWordSpec with Matchers {
  private val executor: ExecutorService = Executors.newFixedThreadPool(2)

  "asyncF" should {
    "evaluate result asynchronously" in {
      val fun = (i: Int) => {
        val res = i.toString
        println()
        res
      }

      val result = Par.asyncF(fun)(1)(executor)

      result.get shouldBe "1"
    }
  }
}
