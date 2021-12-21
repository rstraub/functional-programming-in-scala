package partone.errorhandling

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case Some(v) => Some(fn(v))
    case _ => None
  }

  def flatMap[B](fn: A => Option[B]): Option[B] = map(fn) getOrElse None

  def filter(fn: A => Boolean): Option[A] = flatMap(a => if (fn(a)) Some(a) else None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](option: => Option[B]): Option[B] = this map (Some(_)) getOrElse option
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}
