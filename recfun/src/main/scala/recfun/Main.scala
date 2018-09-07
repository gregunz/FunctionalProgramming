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
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * There are three methods on List[Char] that are useful for this exercise:
   *
   * chars.isEmpty: Boolean returns whether a list is empty
   * chars.head: Char returns the first element of the list
   * chars.tail: List[Char] returns the list without the first element
   *
   * Hint: you can define an inner function if you need to pass extra parameters to your function.
   *
   * Testing: You can use the toList method to convert from a String to a List[Char]: e.g. "(just an) example".toList.
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], open: Int, closed: Int): Boolean =
      if ((closed > open) || chars.isEmpty) open == closed
      else if (chars.head == '(') f(chars.tail, open + 1, closed)
      else if (chars.head == ')') f(chars.tail, open, closed + 1)
      else f(chars.tail, open, closed)
    f(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(coin: Int, moneyLeft: Int, coins: List[Int]): Int =
      if (moneyLeft > 0)
        if (coins.isEmpty)
          loop(coin, moneyLeft - coin, coins)
        else
          loop(coin, moneyLeft - coin, coins) + loop(coins.head, moneyLeft, coins.tail)
      else if (moneyLeft == 0)
        1
      else
        0

    if(coins.isEmpty)
      0
    else
      loop(coins.head, money, coins.tail)
  }

}
