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
      def loop(acc: List[Int], row: Int): List[Int] = {
        if (row == r) {
          acc
        } else {
          val x = if (acc.length > 1) {
            acc.sliding(2).map(_.sum).toList
          } else {
            List.empty[Int]
          }
          loop(1 +: x :+ 1 , row+1)
        }
      }
      loop(List(1), 0)(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(current: List[Char],  acc: List[Char]): Boolean = {
        current match {
          case Nil if acc.isEmpty => true
          case first :: tail if first == '(' => loop(tail, acc :+ '(')
          case first :: tail if first == ')' => if (acc.isEmpty) false else loop(tail, acc.tail)
          case _ :: tail => loop(tail, acc)
        }
      }
      loop(chars, List.empty)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def loop(moneyLeft: Int, coinsLeft: List[Int]): Int = {
        moneyLeft match {
          case mon if mon < 0 => 0
          case mon if mon == 0 => 1
          case _ => coinsLeft match {
            case Nil => 0
            case head :: tail => loop(moneyLeft - head, coinsLeft) + loop(moneyLeft, tail)
          }
        }
      }


      loop(money, coins)
    }
  }
