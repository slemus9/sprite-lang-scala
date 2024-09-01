package sprite.parser

import cats.parse.Parser
import cats.syntax.all.*
import sprite.language.*
import sprite.parser.utils.TokenParser.*

object SpriteTermParser extends SpriteBaseParser:

  lazy val spriteTerm: Parser[SpriteTerm] =
    withAnnotation(
      Parser.defer {
        lambda
          | let
          | lambdaApply
          | baseSpriteTerm
      }
    )

  def withAnnotation(term: Parser[SpriteTerm]): Parser[SpriteTerm] =
    (term ~ (reserved(':').with1 *> SpriteTypeParser.spriteType).?).map {
      case (term, None)            => term
      case (term, Some(annotated)) => SpriteTerm.Annotation(term, annotated)
    }

  lazy val baseSpriteTerm: Parser[SpriteTerm] =
    positiveInt.map(SpriteTerm.Integer.apply)
      | variable
      | spriteTerm.between(reserved('('), reserved(')'))

  lazy val binding: Parser[Bind] =
    (
      identifier <* reserved('='),
      spriteTerm
    ).mapN(Bind.apply)

  lazy val let: Parser[SpriteTerm.Let] =
    (
      binding.between(reserved("let"), reserved(';')),
      spriteTerm
    ).mapN(SpriteTerm.Let.apply)

  lazy val lambda: Parser[SpriteTerm.Lambda] =
    (
      identifier.rep.between(reserved('\\'), reserved("->")),
      spriteTerm
    ).mapN(SpriteTerm.Lambda.apply)

  lazy val lambdaApply: Parser[SpriteTerm] =
    baseSpriteTerm.rep.map { terms =>
      terms.reduceLeft(SpriteTerm.LambdaApply.apply)
    }
end SpriteTermParser
