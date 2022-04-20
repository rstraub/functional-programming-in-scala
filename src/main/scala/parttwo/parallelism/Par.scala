package parttwo.parallelism

import java.util.concurrent._
import scala.List

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled: Boolean = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

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

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = as.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](items: List[A])(predicate: A => Boolean): Par[List[A]] = {
    val parItems: List[Par[List[A]]] = items.map(asyncF(item => if (predicate(item)) List(item) else List.empty))
    map(sequence(parItems))(_.flatten)
  }

  def sequence[A](pars: List[Par[A]]): Par[List[A]] =
    pars.foldRight[Par[List[A]]](unit(List.empty))((h, t) => map2(h, t)(_ :: _))

  def totalWords(paragraphs: List[String]): Par[Int] = {
    val parWordCount: Par[List[Int]] = parMap(paragraphs)(_.split(" ").length)
    map(parWordCount)(_.sum)
  }

  def choiceN[A](number: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(number)(n => choices(n))

  def choice[A](condition: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    chooser(condition)(c => if (c) ifTrue else ifFalse)

  def chooser[A, B](par: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val k = run(es)(par).get()
    run(es)(choices(k))
  }
}
