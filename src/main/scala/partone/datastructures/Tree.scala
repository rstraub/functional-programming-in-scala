package partone.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Trunk extends Tree[Nothing]

object Tree {
  def max(tree: Tree[Int]): Int = tree match {
    case Trunk => 0
    case Leaf(v) => v
    case Branch(l, r) => max(l) max max(r)
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Trunk => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def apply[A](as: A*): Tree[A] = as match {
    case as if as.isEmpty => Trunk
    case as if as.size == 1 => Leaf(as.head)
    case as if as.size % 2 == 0 => Branch(apply(as.take(as.size / 2): _*), apply(as.takeRight(as.size / 2): _*))
    case as if as.size % 2 == 1 => Branch(apply(as.take(as.size / 2 + 1): _*), apply(as.takeRight(as.size / 2): _*))
  }
}
