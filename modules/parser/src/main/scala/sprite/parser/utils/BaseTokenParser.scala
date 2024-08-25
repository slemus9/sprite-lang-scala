package sprite.parser.utils

import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, digit}
import sprite.parser.utils.TokenParser.tokenized

trait BaseTokenParser:

  val identifier: TokenParser[String] =
    (alpha ~ alpha.orElse(digit).rep0).map { (c, s) =>
      (c :: s).mkString
    }.tokenized

  val positiveInt: TokenParser[Int] =
    digit.rep.mapFilter(_.toList.mkString.toIntOption).tokenized

  val bool: TokenParser[Boolean] =
    Parser.fromStringMap(Map("true" -> true, "false" -> false)).tokenized
