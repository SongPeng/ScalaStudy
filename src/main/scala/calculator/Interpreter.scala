package calculator

import calculator.Evaluator.{Term, VarNameInValid, SyntaxError, Keyword}

/**
 * User: sun-april
 * Date: 10-12-26
 * Time: 下午10:10
 */

import org.slf4j.LoggerFactory
import Evaluator._
import java.lang.{Double => JDouble}
import MyToken._

class Interpreter(line: String) {

  val log = LoggerFactory.getLogger(getClass)

  var pointer = -1;
  var fp = 0;
  val buffer = new StringBuilder

  private def nextChar() = {
    pointer += 1
    fp = pointer
    val ch = line.charAt(pointer)
    buffer.append(ch)
    ch
  }

  private def forward(): Char = {
    fp += 1
    val ch = line.charAt(fp)
    ch
  }

  private def clearBuffer(ignoreSpace: Boolean = true) {
    pointer = fp - 1
    fp = pointer

    if (ignoreSpace)
      ignoreWhiteSpace()

    buffer.clear
  }

  private def ignoreWhiteSpace() {
    while (Character.isWhitespace(nextChar)) {}
    pointer -= 1
    fp = pointer
  }

  private def nextToken(condition: Char => Boolean): String = {
    var ch = forward()
    while (condition(ch)) {
      buffer.append(ch)
      ch = forward()
    }

    log.debug("token : {}", buffer.toString)
    buffer.toString
  }

  private def isDefinition(): Boolean = {
    val symbol = nextToken(Character.isLetterOrDigit)

    findSymbol(symbol) match {
      case Some(Keyword(LET)) =>
        clearBuffer()
        true
      case None => false
    }
  }

  private def isLetterOrDigitOrUnderscore(ch: Char) = Character.isLetterOrDigit(ch) || ch == UNDERSCORE

  private def isLetterOrUnderscore(ch: Char) = Character.isLetter(ch) || ch == UNDERSCORE

  private def isDigit(ch: Char) = Character.isDigit(ch)

  private def isEIgnoreCase(ch: Char) = ch == E

  private def installVar() = {

    val id = nextToken(isLetterOrDigitOrUnderscore)

    if (id.isEmpty)
      throw new SyntaxError("variable name expcted!")

    findSymbol(id) match {
      case Some(Keyword(word)) =>
        throw new VarNameInValid(id, "key word is preserved")
      case None =>
        val first: Char = id.charAt(0)
        if (!isLetterOrUnderscore(first))
          throw new VarNameInValid(id, "vars must start with letter or _")
    }

    clearBuffer()

    val assign = forward

    assign match {
      case EQ =>
        clearBuffer()
        Assignment(id, expression())
      case _ => throw new SyntaxError(String.format("`%s expexted, but `%s found", EQ.toString, assign.toString))
    }
  }

  private def factor(): Term = {
    val c = nextChar()
    c match {
    // braces
      case LB =>
        clearBuffer()
        val exp = expression()
        val n = forward
        n match {
          case RB =>
            clearBuffer()
            exp
          case _ => throw new SyntaxError(String.format("`%s expected, but `%s found", RB.toString, n.toString))
        }
      // variable
      case ch if isLetterOrUnderscore(ch) =>
        Var(nextToken(isLetterOrDigitOrUnderscore))
      case ch if isDigit(ch) =>
        val number = new StringBuilder
        number append nextToken(isDigit)
        clearBuffer(false) // inner digit,space not allow
        optPoint(number)
        optE(number)
        Num(JDouble.parseDouble(number.toString))
      case _ => debug_!
    }

  }

  def optPoint(buffer: StringBuilder) {
    forward match {
      case POINT =>
        buffer.append(POINT)
        clearBuffer(false) // inner digit,space not allow
        buffer append nextToken(isDigit)
        clearBuffer(false)
      case _ =>
    }
  }

  def optE(buffer: StringBuilder) {
    forward match {
      case E =>
        buffer append nextToken(s => '+' == s || '+' == s)
        clearBuffer(false)
        buffer append nextToken(isDigit)
        clearBuffer()
      case _ =>
    }
  }


  private def term(): Term = {
    val left = factor()
    left
  }

  private def expression(): Term = {
    val left = term()
    left
  }

  private def debug_! = Debug(line.substring(pointer))

  def process(): Term = {
    clearBuffer()
    val ch = nextChar
    if (!Character.isLetter(ch)) {
      expression()
    }
    if (isDefinition()) {
      installVar()
    } else {
      expression()
    }
  }
}