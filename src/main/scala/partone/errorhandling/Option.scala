package partone.errorhandling

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(fn(v))
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
