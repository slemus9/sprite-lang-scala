package sprite.parser

import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, digit}
import sprite.language.*
import sprite.parser.utils.BaseTokenParser
import sprite.parser.utils.TokenParser
import sprite.parser.utils.TokenParser.tokenized

trait SpriteBaseParser extends BaseTokenParser:

  val primitiveOperation: TokenParser[PrimitiveOperation] =
    Parser.fromStringMap(PrimitiveOperation.byName).tokenized

  val variable: TokenParser[SpriteTerm] =
    identifier
      .orElse(primitiveOperation.map(_.name))
      .map(SpriteTerm.Var.apply)
      .tokenized

  val baseType: TokenParser[SpriteBaseType] =
    Parser.string("Int").as(SpriteBaseType.Int).tokenized
