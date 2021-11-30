package partone.chaptertwo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.chaptertwo.Currying.{curry, uncurry}

class CurryingSpec extends AnyFlatSpec with Matchers {
    "curry" should "return a partially applied function" in {
        val fn: (Int, Int) => String = (a: Int, b: Int) => (a + b).toString

        val initial = curry(fn)
        val partial = initial(2)
        val result = partial(3)

        result shouldBe "5"
    }

    "uncurry" should "return a single function where all arguments must be supplied" in {
        val fn = (a: Int) => (b: Int) => (a + b).toString

        val result = uncurry(fn)

        result(1, 2) shouldBe "3"
    }
}
