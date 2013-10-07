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
  def pascal(c: Int, r: Int): Int = if(c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]) = {

    def calcdepth(depth : Int, chars : List[Char]) : Int =
    	if(chars.isEmpty || depth < 0) 
    	  depth
    	else
    	  (if(chars.head == '(') calcdepth(depth + 1, chars.tail)
    	   else
  		 	(if(chars.head == ')') calcdepth(depth - 1, chars.tail)
  		 	 else calcdepth(depth, chars.tail)))
  	
  	calcdepth(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  	if(money <= 0 || coins.isEmpty) 0
  	else
  	  (if(money - coins.head == 0) 1
  	   else
  	     (if(money - coins.head < 0) 0
  	      else
  	        countChange(money - coins.head, coins))) + countChange(money, coins.tail)
}
