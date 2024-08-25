package sprite.parser

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.all.*
import sprite.language.SpriteDeclaration
import sprite.parser.utils.TokenParser.{reserved, whitespace}

object SpriteParser extends SpriteBaseParser:

  lazy val spriteDeclarations: Parser[NonEmptyList[SpriteDeclaration]] =
    spriteDeclaration.repSep(reserved(';')).between(whitespace, reserved(';').?)

  lazy val spriteDeclaration: Parser[SpriteDeclaration] =
    superCombinatorType.backtrack | superCombinator

  lazy val superCombinatorType: Parser[SpriteDeclaration.SuperCombinatorType] =
    (identifier <* reserved(':'), SpriteTypeParser.spriteType).mapN(
      SpriteDeclaration.SuperCombinatorType.apply
    )

  lazy val superCombinator: Parser[SpriteDeclaration.SuperCombinator] =
    (
      identifier.rep <* reserved('='),
      SpriteTermParser.spriteTerm
    ).mapN { case (NonEmptyList(name, args), term) =>
      SpriteDeclaration.SuperCombinator(name, args, term)
    }
