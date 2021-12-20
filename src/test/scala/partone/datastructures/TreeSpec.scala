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

  it should "return branch and two leaves given two elements" in {
    Tree(1, 2) shouldBe Branch(Leaf(1), Leaf(2))
  }

  it should "return nested branch given three elements" in {
    Tree(1, 2, 3) shouldBe Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  }

  it should "return nested branches given four elements" in {
    Tree(1, 2, 3, 4) shouldBe Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  }

  it should "return trees of deeper nesting" in {
    Tree(1, 2, 3, 4, 5, 6, 7) shouldBe Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(5), Leaf(6)), Leaf(7)))
  }

  "size (ex 3.25)" should "count all nodes in the tree" ignore {
    Tree.size(Tree((1, 2), (3, 4))) shouldBe 7
  }
}
