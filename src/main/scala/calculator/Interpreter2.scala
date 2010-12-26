package calculator

/**
 * Created by IntelliJ IDEA.
 * User: sun-april
 * Date: 10-12-27
 * Time: 上午12:35
 * To change this template use File | Settings | File Templates.
 */

import calculator.Evaluator.{Term, Num}
import Character._

class Interpreter2(line: String) {

  var begin = 0
  var forward = -1

  private def nextChar(): Char = {
    forward += 1
    line.charAt(forward)
  }

  private def nextToken(condition: Char => Boolean): String = {
    val buffer = new StringBuilder
    var ch = nextChar()
    while (condition(ch)) {
      buffer.append(ch)
      ch = nextChar()
    }
    buffer.toString
  }

  def number(): Term = {

    val n = nextToken(isDigit)


    Num(2)
  }


  def process() = {

  }

}


object Test {

  new Interpreter2("let t = 1").process()

  def main(args: Array[String]) {

  }

}