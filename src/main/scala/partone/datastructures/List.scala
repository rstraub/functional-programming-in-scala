package partone.datastructures

import scala.annotation.tailrec

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, y) => y
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = if (n > 0) drop(tail(list), n - 1) else list

  def dropWhile[A](list: List[A], p: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => if (p(head)) tail else list
  }

  def setHead[A](list: List[A], head: A): List[A] = Cons(head, list)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]
