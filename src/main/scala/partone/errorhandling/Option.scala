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
  def Try[A](a: => A): Option[A] = try Some(a) catch {
    case e: Exception => None
  }

  def traverse[A, B](a: List[A])(fn: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(fn(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(op => op)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))(map2(_, _)(_ :: _))

  def lift[A, B](fn: A => B): Option[A] => Option[B] = _ map fn

  def map2[A, B, C](a: Option[A], b: Option[B])(fn: (A, B) => C): Option[C] =
    a flatMap (av => b map (bv => fn(av, bv)))

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(variances(xs, m)))

  private def variances(xs: Seq[Double], m: Double) = xs.map(variance(m, _))

  private def variance(m: Double, x: Double) = math.pow(deviation(x, m), 2)

  private def deviation(x: Double, mean: Double) = x - mean
}
