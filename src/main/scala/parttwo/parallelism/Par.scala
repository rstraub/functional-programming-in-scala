package parttwo.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](par: Par[A])(f: A => B): Par[B] =
    map2(par, unit(()))((a, _) => f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val fbs = as.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](pars: List[Par[A]]): Par[List[A]] =
    pars.foldRight[Par[List[A]]](unit(List.empty))((h, t) => map2(h, t)(_ :: _))
}
