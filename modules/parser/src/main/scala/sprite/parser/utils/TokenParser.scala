package sprite.parser.utils

import cats.parse.{Parser, Parser0}

opaque type TokenParser[+A] <: Parser[A] = Parser[A]

object TokenParser:

  val whitespace: Parser0[String] =
    Parser.charsWhile0(_.isWhitespace)

  def apply[A](parser: Parser[A]): TokenParser[A] =
    parser <* whitespace

  def reserved(c: Char): TokenParser[Unit] =
    TokenParser(Parser.char(c))

  def reserved(str: String): TokenParser[Unit] =
    TokenParser(Parser.string(str))

  extension [A](parser: Parser[A])
    def tokenized: TokenParser[A] =
      TokenParser(parser)
