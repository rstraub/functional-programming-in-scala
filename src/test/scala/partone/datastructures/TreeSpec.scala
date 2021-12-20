package partone.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {
  "tree" should "return trunk given no elements" in {
    Tree() shouldBe Trunk
  }

  it should "return leaf given single element" in {
    Tree(1) shouldBe Leaf(1)
  }

  "size (ex 3.25)" should "count all nodes in the tree" ignore {
    Tree.size(Tree((1, 2), (3, 4))) shouldBe 7
  }
}
