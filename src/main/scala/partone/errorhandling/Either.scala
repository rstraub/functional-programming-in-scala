package partone.errorhandling

sealed trait Either[+E, +A] {
  def map[B](fn: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(fn(v))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
