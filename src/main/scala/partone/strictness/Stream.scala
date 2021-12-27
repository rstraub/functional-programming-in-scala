package partone.strictness

import partone.strictness.Stream.{cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {
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
