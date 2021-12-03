package partone.gettingstarted

import scala.annotation.tailrec

object IsSorted {
  def isSorted[A](array: Array[A], isOrdered: (A, A) => Boolean): Boolean = {
    val size = array.length

    @tailrec
    def loop(currentIndex: Int, nextIndex: Int): Boolean = {
      val current = array(currentIndex)
      val next = array(nextIndex)

      if (isOrdered(current, next)) {
        if (nextIndex < size - 1)
          loop(currentIndex + 1, nextIndex + 1)
        else true
      } else false
    }

    if (size <= 1) true
    else loop(0, 1)
  }
}
