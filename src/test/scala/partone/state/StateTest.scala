package partone.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import partone.state.State.unit

class StateTest extends AnyFlatSpec with Matchers {
  "unit (ex 6.10)" should "return random of constant" in {
    State.unit(1).run()._1 shouldBe 1
  }

  "map" should "transform value" in {
    unit(1).map(_.toString).run()._1 shouldBe "1"
  }
}
