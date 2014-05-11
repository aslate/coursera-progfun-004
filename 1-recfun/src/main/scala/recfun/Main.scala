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
  def pascal(c: Int, r: Int): Int =
    if (r < 2) 1
    else
    if (c == r || c == 0) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1);


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def search(open: Int, chars: List[Char]): Boolean =
      if (open < 0 || open != 0 && chars.isEmpty) false
      else
      if (open == 0 && chars.isEmpty) true
      else
      if (chars.head == '(') search(open + 1, chars.tail)
      else
      if (chars.head == ')') search(open - 1, chars.tail)
      else
        search(open, chars.tail)

    search(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def sortedCountChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0 else
      if (coins.isEmpty) 0 else
      if (money - coins.head < 0) 0 + countChange(money, coins.tail) else
      if (money - coins.head == 0) 1 + countChange(money - coins.head, coins.tail) else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    sortedCountChange(money, coins.sortWith(_ < _))
  }
}