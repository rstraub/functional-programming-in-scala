package partone.state

import partone.state.State.unit

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def map[B](fn: A => B): State[S, B] =
    flatMap(a => unit(fn(a)))

  def flatMap[B](fn: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    fn(a).run(s1)
  })
}

object State {
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, sas, List()))
  }

  def unit[S, A](c: A): State[S, A] = State(s => (c, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
