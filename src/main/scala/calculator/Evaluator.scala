package calculator

/**
 * User: sun-april
 * Date: 10-12-26
 * Time: 2:22
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


import scala.collection.mutable.Map

object Evaluator {

  val termTable: Map[String, Term] = Map(
    "var" -> Keyword("var")
  )

  def findSymbol(id: String) = termTable get id

  def findVar(id: String) = context get id

  def installVar(id: String, value: Expression) = context.put(id, value)

  implicit def numberToExpr(t: Double): Num = Num(t)

  class SymbolNotDefinedException(symbol: String)
    extends Exception(String.format("symbol `%s not defined", symbol))

  class DividedByZero(position: Int) extends Exception("divisor can't be zero")

  class VarNameInValid(name: String, message: String)
    extends Exception(String.format("variable name `%s is invalid cause : %s", name, message))

  class SyntaxError(msg: String) extends Exception(msg)

  sealed abstract class Term

  abstract class Expression extends Term

  case class Add(left: Expression, right: Expression) extends Expression

  case class Minus(left: Expression, right: Expression) extends Expression

  case class Multiply(left: Expression, right: Expression) extends Expression

  case class Division(left: Expression, right: Expression) extends Expression

  case class Num(value: Double) extends Expression

  case class Var(id: String) extends Expression

  case class Assignment(id: String, value: Expression) extends Expression

  case class Keyword(word: String) extends Term

  case class Debug(remain: String) extends Term

  val context: Map[String, Expression] = Map()

  def evaluate(exp: Expression): Double = exp match {
    case Add(l, r) => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Multiply(l, r) => evaluate(l) * evaluate(r)
    case Division(l, r) => evaluate(l) / evaluate(r)
    case Num(n) => n
    case Var(id) => context.get(id) match {
      case Some(t) => evaluate(t)
      case None => throw new SymbolNotDefinedException(id)
    }
    case Assignment(id, v) =>
      val result = evaluate(v)
      context.put(id, result)
      result
  }

  def main(args: Array[String]) {
    val d = Division(3, Add(2, 8))
    val var1 = Var("var1")
    context += ("var1" -> 3)
    val term = Add(var1, Minus(Multiply(2, 8), Multiply(2, 5)))
    val result = evaluate(term)
  }

}