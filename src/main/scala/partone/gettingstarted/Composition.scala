package partone.gettingstarted

object Composition {
  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))
}
