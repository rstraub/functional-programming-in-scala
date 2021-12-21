package partone.errorhandling

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(fn(v))
  }

  def filter(fn: A => Boolean): Option[A] = this match {
    case Some(v) if fn(v) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
