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
    if(r <= 1 || c <= 0 || (c >= r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceStep(balance: Int, list: List[Char]): Boolean = {
      if(list.isEmpty || balance < 0) balance == 0
      else {
        balanceStep(balance + evalChar(list.head), list.tail)
      }
    }
    
    def evalChar(char: Char) = if(char == '(') 1 else if(char == ')') -1 else 0
    
    balanceStep(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) 0
    else if(money == 0) 1
    else if(money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
