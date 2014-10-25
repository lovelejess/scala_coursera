package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if((c == 0) || (c == r)) 1
    else return pascal(c-1,r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], isOpened: Boolean, isClosed:Boolean): Boolean = chars match {
      case Nil => isOpened && isClosed
      case '('::tail => balanceHelper(tail, true, false)
      case ')'::tail => balanceHelper(tail, isOpened, true)

    }
    balanceHelper(chars,false, false)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
