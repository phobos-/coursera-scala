package recfun

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
    def pascal(c: Int, r: Int): Int = if (c <= 0 || r <= 0) 1 else pascal(c-1,r) * (r+1-c)/c
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def bal(c: List[Char], count: Int): Boolean = c match {
          case Nil => if (count == 0) true else false
          case '(' :: rest => bal(rest, count+1)
          case ')' :: rest => if (count <= 0) false else bal(rest, count-1)
          case _ => bal(c.tail, count)
        }
      bal(chars,0)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
