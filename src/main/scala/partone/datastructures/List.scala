package partone.datastructures

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, y) => y
  }

  def setHead[A](list: List[A], head: A): List[A] = Cons(head, list)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]
