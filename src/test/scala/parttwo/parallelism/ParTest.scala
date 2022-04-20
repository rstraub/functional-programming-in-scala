package parttwo.parallelism

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import parttwo.parallelism.Par.{Par, sortPar, unit}

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

  "parFilter" should {
    "filter items in parallel" in {
      val nums = List(1, 2, 3, 4)
      val isEven = (number: Int) => number % 2 == 0

      val result = runMultiThreaded(Par.parFilter(nums)(isEven))

      result.get() shouldBe List(2, 4)
    }
  }

  "totalWords" should {
    "return total words in paragraphs" in {
      val paragraphs = List(
        "lorum ipsum dorum",                      // 3
        "functional programming in scala",        // 4
        "is tough"                                // 2
      )

      val result = runMultiThreaded(Par.totalWords(paragraphs))

      result.get() shouldBe 9
    }
  }

  "choiceN" should {
    "run the nth option" in {
      val choices = List(unit(4), unit(8), unit(10))
      val result = runMultiThreaded(Par.choiceN(unit(1))(choices))

      result.get() shouldBe 8
    }
  }

  "choice" should {
    "run true option" in {
      val result = runMultiThreaded(Par.choice(unit(true))(unit(1), unit(10)))

      result.get() shouldBe 1
    }

    "run false option" in {
      val result = runMultiThreaded(Par.choice(unit(false))(unit(1), unit(10)))

      result.get() shouldBe 10
    }
  }

  private def runMultiThreaded[A](par: Par[A]) = Par.run(executor)(par)
}
