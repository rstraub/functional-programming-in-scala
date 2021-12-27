package partone.strictness

import scala.annotation.tailrec

sealed trait Stream[+A] {
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

  private def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  private def empty[A](): Stream[A] = Empty
}
