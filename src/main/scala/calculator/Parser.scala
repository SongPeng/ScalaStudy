package calculator

/**
 * User: sun-april
 * Date: 10-12-27
 */

import Character._
import calculator.Evaluator._
import language.implicitConversions

import java.lang.{Double => JDouble}

class ExpressionError(val position: Int, message: String) extends Exception(message)

import Interpreter.EOF

class Parser(line: String) {

  def error(message: String): Exception = new ExpressionError(tokenbegin, message)

  type MatchedChar = (Boolean, Char)

  implicit def match2Bool(m: MatchedChar): Boolean = m._1

  def debug = Debug(line.drop(lexbegin))

  var forward = -1
  var lexbegin = -1
  var tokenbegin = 0


  def runAndReset[R](fun: => R): R = {
    val f = forward
    val l = lexbegin
    val t = tokenbegin


    val r = fun

    forward = f
    lexbegin = l
    tokenbegin = t

    r
  }

  var inBraces = 0

  def nextChar : Char = {
    forward += 1
    if (forward >= line.length)
      EOF
    else
      line.charAt(forward)
  }

  def backward(){
    forward -= 1
    lexbegin = forward
  }

  def stepOver(){ lexbegin = forward }

  def resetForward() { forward = lexbegin }

  def matchChar(condition: Char => Boolean, reset: Boolean => Boolean = (m) => !m): (Boolean, Char) = {
    val ch = nextChar
    val m = condition(ch)
    if (reset(m))
      backward()
    else
      stepOver()

    (m, ch)
  }

  def repeatMatchChar(condition: Char => Boolean)(fun: (Boolean, Char) => Any){

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

  def isVarDef : Boolean = {
    runAndReset[Boolean] {
      matchToken("var", forward = false) && matchChar(isWhitespace, (_ => true))
    }
  }

  def varDef(): Expression = {

    matchToken("var")
    ignoreWhiteSpace()

    val f = forward
    val l = lexbegin
    val t = tokenbegin

    def reset(){
      forward = f
      lexbegin = l
      tokenbegin = t
    }

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

    val isEqSign = matchChar('=' == _)
    if (!isEqSign._1)
      throw error(String.format("`= expexted but found : `%s`", isEqSign._2.toString))

    val id = buffer.toString()

    findSymbol(id) match {
      case Some(Keyword(w)) =>
        reset()
        throw error(String.format("%s is reserved,can not be a variable name", w))
      case _ =>
    }

    Assignment(id, expression())
  }

  def variable(): Expression = {

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
      throw error(String.format("var name `%s` has not been defined", id))
    }

    Var(id)
  }

  def number(): Expression = {
    val buffer = new StringBuilder

    val unary = matchChar(isPlusOrMinus)

    if (unary._1)
      buffer.append(unary._2)

    repeatMatchChar(isDigit) {
      (_, c) =>
        buffer.append(c)
    }

    if (matchChar('.' == _)) {
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
    val value = try {
      JDouble.parseDouble(buffer.toString())
    } catch {
      case e: NumberFormatException => throw error("can't parse the number.")
    }
    Num(value)

  }

  def factor(): Expression = {
    ignoreWhiteSpace()
    if (matchChar(isLetter, (_ => true)))
      variable()
    else if (matchChar('(' == _ )) {

      inBraces += 1

      val exp = expression()

      ignoreWhiteSpace()

      val (m, ch) = matchChar(')' == _)
      if (!m)
        throw error(String.format("`) expected but found : %s", ch.toString))

      inBraces -= 1
      exp
    } else if (matchChar(isDigit, (_ => true)) || matchChar(isPlusOrMinus, (_ => true))) {
      number()
    } else {
      throw error("unkonw symbol.")
    }
  }

  def multiplyOrDivide(): Expression = {
    def isMulOrDiv(c: Char): Boolean = '*' == c || '/' == c

    ignoreWhiteSpace()
    val left = factor()

    var result = left
    var sign = (false, EOF)
    do {
      ignoreWhiteSpace()
      sign = matchChar(isMulOrDiv)
      sign match {
        case (true, '*') => result = Multiply(result, factor())
        case (true, '/') => result = Division(result, factor())
        case (_, EOF) => result
        case (false, ')') if (inBraces > 0) => result
        case (false, '+') => result
        case (false, '-') => result
        case _ => throw error(String.format("`operater expected but found : %s", sign._2.toString))
      }

    } while (sign._1)

    result
  }

  private def isPlusOrMinus(c: Char): Boolean = '+' == c || '-' == c

  def expression(): Expression = {
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
        case (false, ')') if (inBraces > 0) => result
        case (_, EOF) => result
        case _ => throw error(String.format("`operater expected but found : %s", sign._2.toString))
      }
    } while (sign._1)
    result
  }

  def statement(): Expression = {
    ignoreWhiteSpace()
    if (matchChar(isLetter, (_ => true))) {
      if (isVarDef)
        varDef()
      else
        expression()
    }
    else
      expression()

  }


  def process(): Expression = {
    statement()
  }

}