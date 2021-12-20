package partone.datastructures

import scala.annotation.tailrec

object List {
  def doublesToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def incrementByOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def concat[A](list: List[List[A]]): List[A] = foldRight(list, Nil:List[A])(append)

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_, _))

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, _) => foldLeft(l, Nil: List[A])((e, l) => Cons(e, l))
  }

  def lengthLeft(l: List[Int]): Int = foldLeft(l, 0)((_, b) => b + 1)

  def productLeft(nums: List[Double]): Double = foldLeft(nums, 1.0)(_ * _)

  def sumLeft(nums: List[Int]): Int = foldLeft(nums, 0)(_ + _)

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

  @tailrec
  def dropWhile[A](list: List[A])(p: A => Boolean): List[A] = list match {
    case Cons(head, tail) if p(head) => dropWhile(tail)(p)
    case _ => list
  }

  def sum2(nums: List[Int]): Int = foldRight(nums, 0)(_ + _)

  def product2(nums: List[Double]): Double = foldRight(nums, 1.0)(_ * _)

  def foldRight[A, B](list: List[A], initial: B)(f: (A, B) => B): B = list match {
    case Nil => initial
    case Cons(head, tail) => f(head, foldRight(tail, initial)(f))
  }

  @tailrec
  def foldLeft[A, B](list: List[A], initial: B)(f: (A, B) => B): B = list match {
    case Nil => initial
    case Cons(h, t) => foldLeft(t, f(h, initial))(f)
  }

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, b) => b + 1)

  def init[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("Cannot get init for Nil")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def setHead[A](list: List[A], head: A): List[A] = Cons(head, list)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]
