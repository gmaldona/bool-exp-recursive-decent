import scala.io.StdIn._
import scala.language.postfixOps
import scala.util.matching.Regex

/**

 * Grammar:
 *
 *  S  => Assign $ | Query $
 *  Q  => ID ?
 *  A  => ID = E
 *  E  => NOT T E2 | T E2
 *  E2 => AND T E2 | OR T E2 | XOR T E2 | e
 *  T  => ( E ) | F | NOT E
 *  F  > ID | TRUE | FALSE
 **/

// TODO: Fix parenthesis precedence?
// TODO: Fix Not E
// NOTE TO SELF: I wonder if it is negating first or doing the computation and then negating
// p = 1
// q = 0

// (~p | q) => (~1 | 0) => (0 | 0) => 0         # negating first then computation
// (~p | q) => (~1 | 0) => (~1) => 0            # computation and then negation
//

/**
 *  p   q    ~p|q    p&(~p|q)    ~ (p&(~p|q))
 * ------------------------------------------
 * T    T       T       T               F
 * T    F       F       F               T
 * F    T       T       F               T
 * F    F       T       F               T
 *
 * */

class ParseError(msg: String) extends Exception(msg)
sealed trait S2
sealed trait Terminal

abstract class E2(_t: T, _e2: Option[E2]) {
    val t: T = _t
    val e2: Option[E2] = _e2
}

case class S(s2: S2, eol: EOL)
case class Assign(id: Id, e: E) extends S2
case class Query(id: Id) extends S2
case class E(not: Option[NOT], t: T, e2: Option[E2])
case class NOT()
case class AND(_t: T, _e2: Option[E2]) extends E2(_t, _e2)
case class OR(_t: T, _e2: Option[E2]) extends E2(_t, _e2)
case class XOR(_t: T, _e2: Option[E2]) extends E2(_t, _e2)
case class T(not: Option[NOT], e: Option[E], f: Option[F])
case class P(e: E)
case class F(terminal: Terminal)

case class Id(id: String) extends Terminal
case class Bool(bool: Boolean) extends Terminal
case class EOL()

case class Yylex(expression: String) {
    var index: Int = 0
    var id_regex: Regex = "[a-z]".r

    def advance(): Boolean = {
        index = index.+(1)
        ! (index >= expression.length)
    }
    def peak(): Char = expression.charAt(index)
}

class Lexer(yylex: Yylex) {

    def s(): S = {
        if (yylex.expression.contains('?')) {
            return S(query(), EOL())
        }

        if (yylex.expression.contains('=')) {
            return S(assign(), EOL())
        }

        throw new ParseError("Parse error, does not  contain '?' or '='.")
    }

    def query(): Query = {
        yylex.id_regex.findFirstMatchIn(yylex.expression) match {
            case Some(id) => {
                yylex.advance()
                if (yylex.peak() != '?') {
                    throw new ParseError("Parse error, a '?' does not follow a single ID.")
                }
                Query(Id(id.toString()))
            }
            case None => throw new ParseError("Parse error, an ID was not found.")
        }
    }

    def assign(): Assign = {
      yylex.id_regex.findFirstMatchIn(yylex.expression) match {
          case Some(id) => {
              if (! yylex.advance()) {
                  throw new ParseError("Parse error, not a complete expression.")
              }
              if (yylex.peak() != '=') {
                  throw new ParseError("Parse error, a '=' does not follow a single ID.")
              }
              yylex.advance()
              Assign(Id(id.toString()), e())
          }
          case None => throw new ParseError("Parse error, an ID was not found.")
      }
    }

    def e(): E = {
        if (yylex.peak() == '~') {
            yylex.advance()
            val _t = t()
            if (yylex.advance()) {
                val _e2 = e2()
                return E(Some(NOT()), _t, _e2)
            }
            return E(Some(NOT()), _t, None)
        }
        val _t = t()
        if (yylex.advance()) {
            val _e2 = e2()
            return E(None, _t, _e2)
        }

        E(None,_t, None)
    }

    def e2(): Option[E2] = {

        if (yylex.peak() == '&') {
            yylex.advance()
            val _t = t()
            if (yylex.advance()) {
                val _e2 = e2()
                return Some(AND(_t, _e2))
            }
            return Some(AND(_t, None))
        }

        if (yylex.peak() == '|') {
            yylex.advance()
            val _t = t()
            if (yylex.advance()) {
                val _e2 = e2()
                return Some(OR(_t, _e2))
            }
            return Some(OR(_t, None))
        }

        if (yylex.peak() == '^') {
            yylex.advance()
            val _t = t()
            if (yylex.advance()) {
                val _e2 = e2()
                return Some(XOR(_t, _e2))
            }
            return Some(XOR(_t, None))
        }

        None
    }
    def t(): T = {
        if (yylex.peak() == '(') {
            yylex.advance()
            val t = T(None, Some(e()), None)
            return t
        }

        if (yylex.peak() == '~') {
            yylex.advance()
            val t = T(Some(NOT()), Some(e()), None)
            return t
        }

        T(None, None, Some(f()))
    }

    def p(): P = P(e())

    def f(): F = {
        if (yylex.peak().isDigit) {
            yylex.peak().asDigit match {
                case 0 => return F(Bool(false))
                case 1 => return F(Bool(true))
                case _ => throw new ParseError("Parse error, given not true or false.")
            }
        }

        if (yylex.peak().isLower) {
            return F(Id(yylex.peak().toString))
        }

        throw new ParseError("Parse error, could not parse terminal.")
    }

}

object Parser {

    var map: Map[String, Boolean] = Map("a" -> true)

    

    def main(args: Array[String]): Unit = {
        while (true) {
            val yylex: Yylex = Yylex(readLine().replace(" ", ""))
            val lexer: Lexer = new Lexer(yylex)

            val parse = lexer.s()
            parse.s2 match {
                case a: Assign => map = map + (a.id.id -> evalE(a.e))
                case q: Query => if (map(q.id.id)) println(1) else println(0)
            }

        }

    }

    def evalOP(bool: Boolean, e2: E2): Boolean = {
        val result = e2 match {
            case _: AND => evalT(e2.t) & bool
            case _: OR  => evalT(e2.t) | bool
            case _: XOR => evalT(e2.t) ^ bool
        }
        result
    }

    def evalOP(bool: Boolean, prev_result: Boolean, e2: E2): Boolean = {
        val result = e2 match {
            case _: AND => bool & prev_result
            case _: OR  => bool | prev_result
            case _: XOR => bool | prev_result
        }
        result
    }

    def evalE(_e: E): Boolean = {

        /**
         *
         * c = 1 & 0 | a & b
         *
         * ---------------------
         * |      & b
         * ---------------------    1) result = a & b
         * |      | a
         * ---------------------    2) result = 0 | result
         * |      & 0
         * ---------------------    3) result = 1 & result
         * |      1
         * ---------------------    4) result
         * */

        var l: Boolean =  evalT(_e.t)

        var e2: Option[E2] = _e.e2
        if (e2.isEmpty) {
            _e.not match {
                case Some(_) => return ! l
                case None      => return l
            }
        }

        _e.not match {
            case Some(_) => l = ! l
            case None    =>
        }

        var e2_stack: List[E2] = List()
        while (e2.nonEmpty) {
            e2_stack = e2.get :: e2_stack
            e2 = e2.get.e2
        }

        var result: Option[Boolean] = None
        if (e2_stack.length == 1) {
            return evalOP(l, e2_stack.head)
        }

        while (e2_stack.nonEmpty) {
            if (e2_stack.length == 1) {
                result = Some(evalOP(l, result.get, e2_stack.head))
            } else {
                val _t = evalT(e2_stack.drop(1).head.t)
                if (result.isEmpty) {
                    result = Some(evalOP(_t, e2_stack.head))
                } else {
                    result = Some(evalOP(_t, result.get, e2_stack.head))
                }
            }
            e2_stack = e2_stack.drop(1)
        }

        result.get
    }

    def evalT(t: T): Boolean = {
        t.f match {
            case Some(f) => return evalF(f)
            case None    =>
        }

        t.e match {
            case Some(e) => {
                t.not match {
                    case Some(_) => {
                        val result = ! evalE(e)
                        println(result)
                        return result
                    }
                    case None      => {
                        val result = evalE(e)
                        println(result)
                        return result
                    }
                }
            }
            case None    =>
        }

        throw new ParseError("Could not evaluate T.")
    }

    def evalF(f: F): Boolean = {
        f.terminal match {
            case _: Id   => map.get(f.terminal.asInstanceOf[Id].id) match {
                case Some(bool) => bool
                case None => throw new ParseError("Could not find " + f.terminal.asInstanceOf[Id].id + " in table.")
            }
            case _: Bool => f.terminal.asInstanceOf[Bool].bool
        }
    }

}
