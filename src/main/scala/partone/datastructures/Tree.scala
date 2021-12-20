package partone.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Trunk extends Tree[Nothing]

object Tree {
  def size[A](tree: Tree[A]): Int = 1

  def apply[A](as: A*): Tree[A] = as match {
    case as if as.size == 4 => Branch(apply(as.take(2):_*), apply(as.takeRight(2):_*))
    case as if as.size == 3 => Branch(apply(as.take(2):_*), apply(as(2)))
    case as if as.size == 2 => Branch(apply(as.head), apply(as(1)))
    case as if as.size == 1 => Leaf(as.head)
    case _ => Trunk
  }
}
