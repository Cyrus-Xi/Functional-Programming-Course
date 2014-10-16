package recfun

object experiment {
  def balance(chars: List[Char]): Boolean = {
    // Empty string/list is balanced.
    def accum(list: List[Char], sum: Int): Int = {
      list match {
        case Nil => sum
        case '(' :: _ => accum(list.tail, sum + 1)
        case ')' :: _ => accum(list.tail, sum - 1)
        case _ :: _ => accum(list.tail, sum)
      }
    }
    if (chars.isEmpty) true else accum(chars, 0) == 0
  }                                               //> balance: (chars: List[Char])Boolean
  
  balance("(just an) example".toList)             //> res0: Boolean = true
  balance("(just an)) example".toList)            //> res1: Boolean = false
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
                                                  //> res2: Boolean = true
  balance("(if (zero? x) max (/ 1 x))".toList)    //> res3: Boolean = true
  balance(":-)".toList)                           //> res4: Boolean = false
  balance("())(".toList)                          //> res5: Boolean = true
  
  
  def balance3(chars: List[Char]): Boolean = {
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
  }                                               //> balance3: (chars: List[Char])Boolean
  
  balance3("(just an) example".toList)            //> res6: Boolean = true
  balance3("(just an)) example".toList)           //> res7: Boolean = false
  balance3("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
                                                  //> res8: Boolean = true
  balance3("(if (zero? x) max (/ 1 x))".toList)   //> res9: Boolean = true
  balance3(":-)".toList)                          //> res10: Boolean = false
  balance3("())(".toList)                         //> res11: Boolean = false
  
  def countChange(money: Int, coins: List[Int]): Int = {
    /*
     * 3 base cases: 1 way to make change if 0 money, 0 ways if negative
     * money, and 0 ways if no coin denominations left.
     *
     * Recursive case: either we use the first coin denomination or we don't.
     */
    if (money == 0) 1 else if (money < 0) 0 else if (coins.isEmpty) 0 else {
      countChange(money, coins.tail) + countChange(money-coins.head, coins)
    }
  }                                               //> countChange: (money: Int, coins: List[Int])Int
 
 	countChange(4, List(1, 2))                //> res12: Int = 3
 	countChange(10, List(1, 2, 3))            //> res13: Int = 14
 	countChange(6, List(1, 2, 3))             //> res14: Int = 7
}