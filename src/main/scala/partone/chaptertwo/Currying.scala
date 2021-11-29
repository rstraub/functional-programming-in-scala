package partone.chaptertwo

object Currying {
  def curry[A, B, C](fn: (A, B) => C): A => B => C = a => b => fn(a, b)
}
