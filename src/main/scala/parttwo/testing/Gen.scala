package parttwo.testing

case class Gen[A](value: A) {
  def next: A = value
}

object Gen {
  def choose(start: Int, stopInclusive: Int): Gen[Int] = new Gen(start)
}
