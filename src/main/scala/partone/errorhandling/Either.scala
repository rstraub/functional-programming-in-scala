package partone.errorhandling

sealed trait Either[+E, +A] {
  def map[B](fn: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(fn(v))
  }

  def flatMap[EE >: E, B](fn: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(v) => fn(v)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch {
    case e: Exception => Left(e)
  }
}
