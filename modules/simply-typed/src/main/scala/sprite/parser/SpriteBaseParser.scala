package sprite.parser

import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, digit}
import sprite.language.*
import sprite.parser.TokenParser.tokenized

trait SpriteBaseParser:

  val identifier: TokenParser[String] =
    (alpha ~ alpha.orElse(digit).rep0).map { (c, s) =>
      (c :: s).mkString
    }.tokenized

  val primitiveOperation: TokenParser[PrimitiveOperation] =
    Parser.fromStringMap(PrimitiveOperation.byName).tokenized

  val variable: TokenParser[SpriteTerm] =
    identifier
      .orElse(primitiveOperation.map(_.name))
      .map(SpriteTerm.Var.apply)
      .tokenized

  val positiveInt: TokenParser[Int] =
    digit.repAs[String].mapFilter(_.toIntOption).tokenized

  val bool: TokenParser[Boolean] =
    Parser
      .fromStringMap(Map("true" -> true, "false" -> false))
      .tokenized

  val baseType: TokenParser[SpriteBaseType] =
    Parser.string("Int").as(SpriteBaseType.Int).tokenized
