package partone.gettingstarted

import scala.annotation.tailrec

object Fibonnaci {
  def fibonnaci(numberToFind: Int): Int = {
    @tailrec
    def go(prev: Int, current: Int, number: Int): Int = {
      val value = prev + current
      if (number == numberToFind)
        value
      else
        go(current, value, number + 1)
    }

    numberToFind match {
      case 0 | 1 => numberToFind
      case _ => go(0, 1, 2)
    }
  }
}
