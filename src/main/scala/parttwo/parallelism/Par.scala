package parttwo.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  def asyncF[A, B](f: A => B): A => Par[B] = ???
}
