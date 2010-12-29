package calculator

/**
 * User: sun-april
 * Date: 10-12-26
 * Time: 下午2:22
 *
 * digit := 0-9
 * digits := digit+
 * numbers = digits (. digits)? (E [+-]? digits)?
 *
 * plus := '+'
 *
 * minus := '-'
 *
 * multiply := '*'
 *
 * division := '/'
 *
 * leftBrace := '('
 *
 * rightBrace := ')'
 *
 */

import org.slf4j.{Logger, LoggerFactory}
import collection.mutable.Map

object Evaluator {

  val log = LoggerFactory.getLogger(getClass)

  val termTable: Map[String, Term] = Map(
    "var" -> Keyword("var")
  )

  def findSymbol(id: String) = termTable get id

  def findVar(id: String) = context get id

  def installVar(id: String, value: Term) = context.put(id, value)

  implicit def numberToTerm(t: Double): Num = Num(t)

  class SymbolNotDefinedException(symbol: String) extends Exception(String.format("symbol `%s not defined", symbol))

  class DividedByZero(position: Int) extends Exception("divisor can't be zero")

  class VarNameInValid(name: String, message: String) extends Exception(String.format("variable name `%s is invalid cause : %s", name, message))

  class SyntaxError(msg: String) extends Exception(msg)

  abstract class Term

  case class Add(left: Term, right: Term) extends Term

  case class Minus(left: Term, right: Term) extends Term

  case class Multiply(left: Term, right: Term) extends Term

  case class Division(left: Term, right: Term) extends Term

  case class Num(value: Double) extends Term

  case class Var(id: String) extends Term

  case class Keyword(word: String) extends Term

  case class Assignment(id: String, value: Term) extends Term

  case class Debug(remain: String) extends Term

  val context: Map[String, Term] = Map()

  def evaluate(exp: Term): Double = exp match {
    case Add(l, r) => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Multiply(l, r) => evaluate(l) * evaluate(r)
    case Division(l, r) => evaluate(l) / evaluate(r)
    case Num(n) => n
    case Var(id) => context.get(id) match {
      case Some(t) => evaluate(t)
      case None => throw new SymbolNotDefinedException(id)
    }
    case Assignment(id, exp) =>
      val result = evaluate(exp)
      context.put(id, result)
      result
  }

  def main(args: Array[String]) {
    val d = Division(3, Add(2, 8))
    val var1 = Var("var1")
    context += ("var1" -> 3)
    val term = Add(var1, Minus(Multiply(2, 8), Multiply(2, 5)))
    val result = evaluate(term)
    log.info("term {} result is {}", term, result)
  }

}