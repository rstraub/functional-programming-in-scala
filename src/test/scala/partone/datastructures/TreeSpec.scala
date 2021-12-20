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

  "size (ex 3.25)" should "count all nodes in the tree" in {
    Tree.size(Tree(1, 2)) shouldBe 3
  }

  it should "count empty tree as 0 nodes" in {
    Tree.size(Tree()) shouldBe 0
  }

  it should "count a leaf as 1 node" in {
    Tree.size(Tree(1)) shouldBe 1
  }

  "max (ex 3.26)" should "return maximum of all nodes in the tree" in {
    Tree.max(Tree(4, 10, 2)) shouldBe 10
  }

  it should "return 0 given an empty tree" in {
    Tree.max(Tree()) shouldBe 0
  }

  it should "return value of leaf given a leaf" in {
    Tree.max(Tree(1)) shouldBe 1
  }

  "depth (ex 3.27)" should "return the maximum path length in the tree" in {
    Tree.depth(Tree(1, 2, 3)) shouldBe 3
  }

  it should "return 0 given empty tree" in {
    Tree.depth(Tree()) shouldBe 0
  }

  it should "return 1 given leaf" in {
    Tree.depth(Tree(1)) shouldBe 1
  }

  it should "return 2 given a branch with leaves" in {
    Tree.depth(Tree(1, 2)) shouldBe 2
  }
}
