package partone.errorhandling

sealed trait Either[+E, +A] {
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)  => b
      case Right(v) => Right(v)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(fn: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield fn(aa, bb)

  def map[B](fn: A => B): Either[E, B] = this match {
    case Left(v)  => Left(v)
    case Right(v) => Right(fn(v))
  }

  def flatMap[EE >: E, B](fn: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(v) => fn(v)
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = try Right(a)
  catch {
    case e: Exception => Left(e)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def traverse[E, A, B](
      as: List[A]
  )(fn: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) =>
      fn(h).map2(t)(_ :: _)
    )
}
