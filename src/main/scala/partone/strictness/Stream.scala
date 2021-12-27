package partone.strictness

sealed trait Stream[+A]

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
