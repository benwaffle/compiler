import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

package lexer {

  sealed trait Token

  case class Operator(op: Char) extends Token
  case class Number(value: Int) extends Token

  object Lexer extends RegexParsers {
    def operator: Parser[Operator] = {
      "[+-]".r ^^ { c => Operator(c.charAt(0)) }
    }

    def number: Parser[Number] = {
      "\\d+".r ^^ { n => Number(n.toInt) }
    }

    def tokens: Parser[List[Token]] = {
      phrase(rep1(number | operator))
    }

    def apply(code: String) = {
      parse(tokens, code) match {
        case NoSuccess(msg, _) => Left(msg)
        case Success(result, _) => Right(result)
      }
    }
  }

}

package parser {
  import lexer.{Token, Lexer}

  sealed trait Ast
  case class Operation(left: Ast, op: Char, right: Ast) extends Ast
  case class Number(value: Int) extends Ast

  object MyParser extends Parsers {
    override type Elem = Token

    def number: Parser[Number] = {
      accept("number literal", { case lexer.Number(n) => Number(n) })
    }

    def operation: Parser[Operation] = {
      (Lexer.number ~ Lexer.operator ~ Lexer.number) ^^ {
        case a ~ o ~ b =>
          Operation(Number(a), o, Number(b))
      }
    }

    def all = phrase(rep1(operation | number))


    def apply(tokens: Seq[Token]) = {
      val reader = new TokenReader(tokens)
      all(reader) match {
        case NoSuccess(msg, _) => Left(msg)
        case Success(result, _) => Right(result)
      }
    }
  }

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
    override def pos: Position = NoPosition
    override def atEnd: Boolean = tokens.isEmpty
  }
}

object Main extends App {
  println(for {
    tokens <- lexer.Lexer("1 + 2").right
    ast <- parser.MyParser(tokens).right
  } yield ast)
}