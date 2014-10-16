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
    // Sides are always 1. If not a 1, then return sum of 2 "parents."
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iter(list: List[Char], sum: Int): Boolean = {
      list match {
        case Nil => sum == 0
        case '(' :: tail => iter(list.tail, sum + 1)
        // Handle edge case where correct number but unbalanced.
        case ')' :: tail if (sum < 1) => false
        case ')' :: tail => iter(list.tail, sum - 1)
        case _ :: tail => iter(list.tail, sum)
      }
    }
    // Empty string/list is balanced.
    if (chars.isEmpty) true else iter(chars, 0)
  }

  /**
   * Exercise 3
   *
   * 3 base cases: 1 way to make change if 0 money, 0 ways if negative
   * money, and 0 ways if no coin denominations left.
   *
   * Recursive case: either we use the first coin denomination or we don't.
   * Note that order doesn't matter.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else if (money < 0) 0 else if (coins.isEmpty) 0 else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
