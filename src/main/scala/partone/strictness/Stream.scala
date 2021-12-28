package partone.strictness

import partone.strictness.Stream.{cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]())((a, acc) => cons(f(a), acc))

  def headOption(): Option[A] = foldRight(None: Option[A])((a: A, _) => Some(a))

  def takeWhileZ(p: A => Boolean): Stream[A] =
    foldRight(empty[A]())((a, b) => if (p(a)) cons(a, b) else empty())

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](z: => B)(fn: (A, => B) => B): B =
    this match {
      case Cons(h, t) => fn(h(), t().foldRight(z)(fn))
      case _ => z
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => Empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => Empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty())
    case _ => Empty
  }

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }
    }

    go(this, List()).reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty()
    else cons(as.head, apply(as.tail: _*))

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A](): Stream[A] = Empty
}
