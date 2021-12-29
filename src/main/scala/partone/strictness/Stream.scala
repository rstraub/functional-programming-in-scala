package partone.strictness

import partone.strictness.Stream.{cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]())((e, acc) => f(e) append acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((e, acc) => cons(e, acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]())((e, acc) => if (f(e)) cons(e, acc) else acc)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]())((e, acc) => cons(f(e), acc))

  def exists(p: A => Boolean): Boolean = foldRight(false)((e, acc) => p(e) || acc)

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
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) {
    case (n0, n1) => Some((n0, (n1, n0 + n1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty()
    }
  }

  def empty[A](): Stream[A] = Empty

  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = cons(n1, go(n2, n1 + n2))

    go(0, 1)
  }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def constant[A](c: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => c, () => tail)
    tail
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty()
    else cons(as.head, apply(as.tail: _*))
}
