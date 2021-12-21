package partone.errorhandling

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case Some(v) => Some(fn(v))
    case _ => None
  }

  def flatMap[B](fn: A => Option[B]): Option[B] = map(fn) getOrElse None

  def filter(fn: A => Boolean): Option[A] = flatMap(a => if(fn(a)) Some(a) else None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
}
case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]
