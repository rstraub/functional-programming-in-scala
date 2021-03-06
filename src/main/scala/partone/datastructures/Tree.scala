package partone.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Trunk extends Tree[Nothing]

object Tree {
  def mapViaFold[A, B](tree: Tree[A])(fn: A => B): Tree[B] =
    fold(tree)(a => Leaf(fn(a)): Tree[B])(Branch(_, _))

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ max _)

  def maxViaFold(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def fold[A, B](tree: Tree[A])(fn: A => B)(bb: (B, B) => B): B = tree match {
    case Leaf(v)             => fn(v)
    case Branch(left, right) => bb(fold(left)(fn)(bb), fold(right)(fn)(bb))
  }

  def map[A, B](tree: Tree[A])(fn: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(fn), map(r)(fn))
    case Leaf(v)      => Leaf(fn(v))
    case _            => Trunk
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + depth(l) max depth(r)
    case _            => 0
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(v)      => v
    case Branch(l, r) => max(l) max max(r)
    case _            => 0
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
    case _            => 0
  }

  def apply[A](as: A*): Tree[A] = as match {
    case as if as.isEmpty   => Trunk
    case as if as.size == 1 => Leaf(as.head)
    case as if as.size % 2 == 0 =>
      Branch(
        apply(as.take(as.size / 2): _*),
        apply(as.takeRight(as.size / 2): _*)
      )
    case as if as.size % 2 == 1 =>
      Branch(
        apply(as.take(as.size / 2 + 1): _*),
        apply(as.takeRight(as.size / 2): _*)
      )
  }
}
