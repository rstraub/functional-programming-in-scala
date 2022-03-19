package parttwo.parallelism

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import parttwo.parallelism.Par.{Par, sortPar}

import java.util.concurrent.{ExecutorService, Executors}

class ParTest extends AnyWordSpec with Matchers {
  private val executor: ExecutorService = Executors.newFixedThreadPool(2)

  "asyncF" should {
    "evaluate result asynchronously" in {
      val fun = (i: Int) => {
        val res = i.toString
        println(i)
        res
      }

      val result = runMultiThreaded(Par.asyncF(fun)(1))

      result.get shouldBe "1"
    }
  }

  "sortPar" should {
    "sort list of ints" in {
      val nums = Par.lazyUnit(List(9, 1, 7, 3, 5))
      val result = runMultiThreaded(sortPar(nums))

      result.get shouldBe List(1, 3, 5, 7, 9)
    }
  }

  "parMap" should {
    "run list of computations in parallel" in {
      val nums = List(1, 2, 3, 4)
      val fn = (i: Int) => i.toString

      val result = runMultiThreaded(Par.parMap(nums)(fn))

      result.get shouldBe List("1", "2", "3", "4")
    }
  }

  private def runMultiThreaded[A](par: Par[A]) = Par.run(executor)(par)
}
