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
    def pascal(c: Int, r: Int): Int = {
      if ((c==0) || (c==r)) 1 else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char],count: Int): Int = {
      if (count < 0) 10
      else
        if (chars.isEmpty) count
        else
          if (chars.head == '(') balanceIter(chars.tail,count+1)
          else
            if (chars.head == ')') balanceIter(chars.tail,count-1)
            else balanceIter(chars.tail,count)
    }
    if (balanceIter(chars,0) > 0) false
    else true
  }

  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def itercountChange(m: Int, cs: List[Int], cnt: Int): Int =
      if (m < 0) cnt //Not a change, keep cnt
      else if (cs.isEmpty) {
        if (m == 0) cnt + 1 else cnt // plus cnt if find a change
      }
      else itercountChange(m, cs.tail, cnt) +
        itercountChange(m - cs.head, cs, cnt)
    itercountChange(money, coins, 0)
  }
}
