package partone.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Leaf[A], right: Leaf[A]) extends Tree[A]
case object Trunk extends Tree[Nothing]

object Tree {
  def size[A](tree: Tree[A]): Int = 1


  def apply[A](as: A*): Tree[A] = as match {
    case as if as.size == 1 => Leaf(as.head)
    case _ => Trunk
  }
}
