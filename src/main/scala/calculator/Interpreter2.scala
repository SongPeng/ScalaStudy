package calculator

/**
 * User: sun-april
 * Date: 10-12-27
 * Time: 上午12:35
 */

import Character._
import calculator.Evaluator._

import java.lang.{Double => JDouble}

case class Debug(remain: String) extends Term


class ExpressionError(val position: Int, message: String) extends Exception(message)

object MyToken {
  val EOF = '\u0003'
  val EQ = '='
  val PLUS = '+'
  val MINUS = '-'
  val MULTIPLY = '*'
  val DIVISION = '/'

  val LB = '('
  val RB = ')'
  val UNDERSCORE = '_'

  val POINT = '.'
  val E = 'E'


  val LET = "let"
  val VAR = "var"


}

import MyToken._

class Interpreter2(line: String) {

  def error(message: String): Exception = new ExpressionError(tokenbegin, message)

  type MatchedChar = (Boolean, Char)

  implicit def match2Bool(m: MatchedChar): Boolean = m._1

  def debug = Debug(line.drop(lexbegin))


  var forward = -1
  var lexbegin = -1
  var tokenbegin = 0

  def nextChar(): Char = {
    forward += 1
    if (forward >= line.length)
      EOF
    else
      line.charAt(forward)
  }

  def backward() = {
    forward -= 1
    lexbegin = forward
  }

  def stepOver() = {
    lexbegin = forward
  }

  def resetForward() = forward = lexbegin

  def nextToken(condition: Char => Boolean): String = {
    val buffer = new StringBuilder
    var ch = nextChar()
    while (condition(ch)) {
      buffer.append(ch)
      ch = nextChar()
    }
    buffer.toString
  }

  def matchChar(condition: Char => Boolean, reset: Boolean => Boolean = (m) => !m): (Boolean, Char) = {
    val ch = nextChar
    val m = condition(ch)
    if (reset(m))
      backward()
    else
      stepOver()

    (m, ch)
  }

  def matchChar(condition: Char => Boolean, reset: Boolean = true, stepThrough: Boolean = false) = {
    val ch = nextChar
    val m = condition(ch)
    if (!m && reset)
      backward
    else if (stepThrough)
      stepOver
    else
      resetForward()

    (m, ch)

  }

  def repeatMatchChar(condition: Char => Boolean)(fun: (Boolean, Char) => Any) = {

    var m = matchChar(condition)
    while (m._1) {
      fun(m._1, m._2)
      m = matchChar(condition)
    }
  }

  def matchToken(str: String, forward: Boolean = true): Boolean = {
    for (c <- str) {
      val next = nextChar
      if (next != c) {
        resetForward()
        return false
      }
    }

    if (forward)
      stepOver()

    true
  }

  def ignoreWhiteSpace() {
    while (matchChar(isWhitespace)) {}
    tokenbegin = lexbegin
  }

  @deprecated
  def matchSeq(str: String): Boolean = {

    assert(!str.isEmpty, "matching str is empty")

    var stack = str
    var head = ' '
    var ch = ' '
    do {
      head = stack.charAt(0)
      ch = nextChar()
      stack = stack.substring(1)
    } while (head == ch && !stack.isEmpty)

    val m = stack.isEmpty
    if (!m)
      resetForward()
    else
      stepOver()

    m
  }

  def matchOneOf(stack: Set[Char]): Boolean = {
    stack.exists(nextChar ==)
  }


  def isVarDef(): Boolean = {
    matchToken("var", false) && matchChar(isWhitespace, (_ => true))
  }

  def varDef(): Term = {

    matchToken("var")
    ignoreWhiteSpace()

    val (m, firstChar) = matchChar(isLetter)

    if (!m) {
      throw error("variable name must start with letter")
    }

    val buffer = new StringBuilder
    buffer.append(firstChar)

    repeatMatchChar(isLetterOrDigit) {
      (_, c) => buffer.append(c)
    }

    ignoreWhiteSpace()

    val isEqSign = matchChar('=' ==)
    if (!isEqSign._1)
      throw error(String.format("`= expexted but found : `%s`", isEqSign._2.toString))

    Assignment(buffer.toString, expression())
  }

  def variable(): Term = {

    def varDefined(id: String): Boolean = {
      findVar(id) match {
        case None => false
        case Some(v) => true
      }
    }
    val buffer = new StringBuilder
    repeatMatchChar(isLetterOrDigit) {
      (_, c) => buffer.append(c)
    }

    val id = buffer.toString()

    if (!varDefined(id)) {
      throw error(String.format("var name `%s` not defined", id))
    }

    Var(buffer.toString)
  }

  def number(): Term = {
    val buffer = new StringBuilder

    repeatMatchChar(isDigit) {
      (_, c) => buffer.append(c)
    }

    if (matchChar('.' ==)) {
      buffer.append('.')
      repeatMatchChar(isDigit) {
        (_, c) => buffer.append(c)
      }
    }

    if (matchChar(c => c == 'E' || c == 'e')) {
      buffer.append('E')
      val sign = matchChar(c => '+' == c || '-' == c)
      if (sign)
        buffer.append(sign._2)

      repeatMatchChar(isDigit) {
        (_, c) => buffer.append(c)
      }
    }

    Num(JDouble.parseDouble(buffer.toString))

  }

  def factor(): Term = {
    ignoreWhiteSpace()
    if (matchChar(isLetter, (_ => true)))
      variable()
    else if (matchChar('(' ==)) {
      val exp = expression()
      ignoreWhiteSpace()
      val (m, ch) = matchChar(')' ==)
      if (!m)
        throw error(String.format("`) expected but found : %s", ch.toString))
      exp
    } else if (matchChar(isDigit, (_ => true))) {
      number()
    } else {
      throw error("unkonw symbol.")
    }
  }

  def multiplyOrDivide(): Term = {
    def isMulOrDiv(c: Char): Boolean = '*' == c || '/' == c

    ignoreWhiteSpace()
    val left = factor()

    var result = left
    var sign = (false, EOF)
    do {
      ignoreWhiteSpace()
      sign = matchChar(isMulOrDiv)
      sign match {
      //case (false, c) => throw new SyntaxError(String.format("`* or `/ expected buf found : %s", c.toString))
        case (true, '*') => result = Multiply(result, factor())
        case (true, '/') => result = Division(result, factor())
        case _ => result
      }

    } while (sign._1)

    result
  }

  def expression(): Term = {

    def isPlusOrMinus(c: Char): Boolean = '+' == c || '-' == c

    ignoreWhiteSpace()

    val left = multiplyOrDivide()

    var result = left
    var sign = (false, EOF)
    do {
      ignoreWhiteSpace()
      sign = matchChar(isPlusOrMinus)
      sign match {
        case (true, '+') => result = Add(result, multiplyOrDivide())
        case (true, '-') => result = Minus(result, multiplyOrDivide())
        case _ => result
      }
    } while (sign._1)
    result
  }

  def statement(): Term = {
    if (matchChar(isLetter, (_ => true))) {
      if (isVarDef())
        varDef()
      else
        expression()
    }
    else
      expression()

  }


  def process(): Term = {
    statement()
  }

}


object Test {

  new Interpreter2("let t = 1").process()

  def main(args: Array[String]) {

  }

}