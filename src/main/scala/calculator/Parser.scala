package calculator

/**
 * Created by IntelliJ IDEA.
 * User: sun-april
 * Date: 10-12-26
 * Time: 下午3:39
 *
 * letter := [a-zA-Z]
 *
 * letters := letter+
 *
 * variable := [_letter] (digits | letters)?
 *
 * define := 'let' variable '=' expression
 *
 * factor := numbers
 *        := variable
 *        := '(' expression ')'
 *
 * term := factor * factor
 *      := factor / factor
 *
 * expression := term + term
 *            := term - term
 *
 */

import io.Source
import org.slf4j.LoggerFactory
import collection.mutable.Map
import Evaluator._
import annotation.tailrec

object Parser {

  val log = LoggerFactory.getLogger(getClass)

  val NON_EOF = '\\'

  val EOF: Char = '\u0003'

  val stdinReader = Source.stdin.bufferedReader

  @tailrec
  def interpreterLoop(): Unit = {
    val buffer = new StringBuilder()
    var line = ""
    var scan = true
    print("> ")
    while (scan) {
      line = stdinReader.readLine()
      if (!line.trim.isEmpty) {
        if (line.last == NON_EOF)
          buffer.append(line.take(line.length - 1))
        else {
          buffer.append(line).append(EOF)
          scan = false
        }
      }
    }
    val result = try {
      new Interpreter(buffer.toString).process
    } catch {
      case s: Exception => s.getMessage
    }
    println(result)
    buffer.clear()
    interpreterLoop();
  }


  def main(args: Array[String]) {

    interpreterLoop()

  }

}