package partone.chaptertwo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CurrySpec extends AnyFlatSpec with Matchers {
    "curry" should "return a partially applied function" in {
        val initial = Currying.curry((a: Int, b: Int) => (a + b).toString)
        val partial = initial(2)
        val result = partial(3)

        result shouldBe "5"
    }
}
