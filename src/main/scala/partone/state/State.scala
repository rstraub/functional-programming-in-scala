package partone.state

import partone.state.State.unit

case class State[S, +A](run: S => (A, S)) {
  def map[B](fn: A => B): State[S, B] =
    flatMap(a => unit(fn(a)))

  def flatMap[B](fn: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    fn(a).run(s1)
  })
}

object State {
  def unit[S, A](c: A): State[S, A] = State(s => (c, s))
}
