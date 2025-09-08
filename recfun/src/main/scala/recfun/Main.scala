package recfun
import common._

import scala.annotation.tailrec

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
    if (c==0||r==0||c==r) 1 else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {

    def rec(chars: List[Char] , a:Int): Boolean = {
      if (chars.isEmpty) {
        a==0
      }
      else if (a < 0)  false
      else if (chars.head == '(') {

        rec(chars.tail,a+1)
      }
      else if (chars.head == ')') {

        rec(chars.tail,a-1)
      }

      else   rec(chars.tail,a)

    }
    rec(chars,0)
  }

  /**
   * Exercise 3
   */


  def countChange(money: Int, coins: List[Int]): Int = {
    
      if (money == 0) 1
      else if (coins.isEmpty||money<0) 0
      else  countChange(money - coins.head, coins) + countChange(money, coins.tail)

  }

}
