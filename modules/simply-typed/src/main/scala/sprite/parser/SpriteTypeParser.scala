package sprite.parser

import cats.parse.Parser
import cats.syntax.all.*
import sprite.language.*
import sprite.parser.TokenParser.reserved

object SpriteTypeParser extends SpriteBaseParser:

  lazy val spriteType: Parser[SpriteType] =
    val refined = (baseType ~ refinement.?).map {
      case (base, None)      => SpriteType.Base(base)
      case (base, Some(ref)) => SpriteType.RefinedType(base, refinement = ref)
    }

    Parser.defer(functionType.backtrack | refined)

  lazy val functionType: Parser[SpriteType.FunctionType] =
    val types = spriteType.between(reserved('('), reserved(')')) | spriteType

    (
      identifier <* reserved(':'),
      types <* reserved("->"),
      types
    ).mapN(SpriteType.FunctionType.apply)

  lazy val refinement: Parser[Refinement] =
    (
      identifier <* reserved('|'),
      SpriteTermParser.spriteTerm
    ).mapN(Refinement.apply).between(reserved('{'), reserved('}'))
