package sprite.parser

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.all.*
import sprite.language.SpriteDeclaration
import sprite.language.SpriteTerm
import sprite.parser.utils.TokenParser.{reserved, whitespace}

object SpriteParser extends SpriteBaseParser:

  lazy val spriteDeclarations: Parser[NonEmptyList[SpriteDeclaration]] =
    spriteDeclaration.repSep(reserved(';')).between(whitespace, reserved(';').?)

  lazy val spriteDeclaration: Parser[SpriteDeclaration] =
    typeDeclaration.backtrack | termDeclaration

  lazy val typeDeclaration: Parser[SpriteDeclaration.TypeDeclaration] =
    (identifier <* reserved(':'), SpriteTypeParser.spriteType).mapN(
      SpriteDeclaration.TypeDeclaration.apply
    )

  lazy val termDeclaration: Parser[SpriteDeclaration.TermDeclaration] =
    (identifier <* reserved('='), SpriteTermParser.spriteTerm).mapN(
      SpriteDeclaration.TermDeclaration.apply
    )
