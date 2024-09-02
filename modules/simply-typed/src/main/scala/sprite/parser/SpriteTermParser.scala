package sprite.parser

import cats.parse.Parser
import cats.syntax.all.*
import sprite.language.*
import sprite.language.SpriteTerm.*
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

  lazy val terminalTerm: Parser[TerminalTerm] =
    positiveInt.map[IntConst](IntConst.apply) | variable

  lazy val baseSpriteTerm: Parser[SpriteTerm] =
    positiveInt.map(IntConst.apply)
      | variable
      | spriteTerm.between(reserved('('), reserved(')'))

  lazy val binding: Parser[Bind] =
    (
      identifier <* reserved('='),
      spriteTerm
    ).mapN(Bind.apply)

  lazy val let: Parser[Let] =
    (
      binding.between(reserved("let"), reserved(';')),
      spriteTerm
    ).mapN(Let.apply)

  lazy val lambda: Parser[Lambda] =
    (
      identifier.rep.between(reserved('\\'), reserved("->")),
      spriteTerm
    ).mapN(Lambda.apply)

  lazy val lambdaApply: Parser[SpriteTerm] =
    (baseSpriteTerm ~ terminalTerm.rep0).map {
      case (term, Nil) => term
      case (fun, args) => args.foldLeft(fun)(LambdaApply.apply)
    }

end SpriteTermParser
