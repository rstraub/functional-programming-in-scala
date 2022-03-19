package partone.strictness

import partone.strictness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def tails(): Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream.empty()

  def startsWith[B >: A](s: Stream[B]): Boolean =
    !zipWith(s)((a, b) => a == b).exists(_ == false)

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h, t)) =>
        Some((Option.empty[A], Some(h())), (empty[A](), t()))
      case (Cons(h, t), Empty) =>
        Some((Some(h()), Option.empty[B]), (t(), empty[B]()))
      case (Empty, Empty) => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, _), n) if n == 1 => Some((h(), (empty(), 0)))
    case (Cons(h, t), n) if n > 1  => Some((h(), (t(), n - 1)))
    case _                         => None
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _          => None
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]())((e, acc) => f(e) append acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((e, acc) => cons(e, acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]())((e, acc) => if (f(e)) cons(e, acc) else acc)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]())((e, acc) => cons(f(e), acc))

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((e, acc) => p(e) || acc)

  def headOption(): Option[A] = foldRight(None: Option[A])((a: A, _) => Some(a))

  def takeWhileZ(p: A => Boolean): Stream[A] =
    foldRight(empty[A]())((a, b) => if (p(a)) cons(a, b) else empty())

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](z: => B)(fn: (A, => B) => B): B =
    this match {
      case Cons(h, t) => fn(h(), t().foldRight(z)(fn))
      case _          => z
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _                    => Empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1  => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _                    => Empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty())
    case _                    => Empty
  }

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _          => acc
      }
    }

    go(this, List()).reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def onesViaUnfold(): Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](c: A): Stream[A] = unfold(c)(_ => Some((c, c)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) { case (n0, n1) =>
    Some((n0, (n1, n0 + n1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty()
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
