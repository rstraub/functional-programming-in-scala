package partone.errorhandling

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case Some(v) => Some(fn(v))
    case _ => None
  }

  def flatMap[B](fn: A => Option[B]): Option[B] = this match {
    case Some(v) => fn(v)
    case _ => None
  }

  def filter(fn: A => Boolean): Option[A] = this match {
    case Some(v) if fn(v) => this
    case _ => None
  }
}
case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]
