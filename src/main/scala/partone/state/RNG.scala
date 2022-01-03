package partone.state

trait RNG {
  def nextInt(): (Int, RNG)
}

object RNG {
  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = { rng =>
    val (a: A, r1: RNG) = s(rng)
    f(a)(r1)
  }

  def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = rng =>
    flatMap(s)(a => unit(f(a)))(rng)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def intsViaSequence(n: Int): Rand[List[Int]] = sequence(List.fill(n)(nonNegativeInt))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng =>
    fs.foldRight(unit(List[A]()))((ra, l) => map2(ra, l)(_ :: _))(rng)

  def randIntDouble(): Rand[(Int, Double)] = both(nonNegativeInt, double)

  def randDoubleInt(): Rand[(Double, Int)] = both(double, nonNegativeInt)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, rng2) = nonNegativeInt(rng)
    val n = num / (Int.MaxValue.toDouble + 1)
    (n, rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt()
    val positiveInt = if (n == Int.MinValue) Int.MaxValue else math.abs(n)
    (positiveInt, next)
  }

  def doubleViaMap(): Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (n1, rng1) = rng.nextInt()
      val (ns, rng2) = ints(count - 1)(rng1)
      (n1 :: ns, rng2)
    }
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt()
    val (d, r2) = double(r)
    ((i, d), r2)
  }
}

