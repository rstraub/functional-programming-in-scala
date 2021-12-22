package partone.errorhandling

sealed trait Either[+E, +A] {
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(fn: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield fn(aa, bb)

  def map[B](fn: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(fn(v))
  }

  def flatMap[EE >: E, B](fn: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(v) => fn(v)
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch {
    case e: Exception => Left(e)
  }
}
