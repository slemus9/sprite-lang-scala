package sprite.solver.qflia.parser

import cats.parse.Parser
import cats.syntax.all.*
import sprite.parser.utils.BaseTokenParser
import sprite.parser.utils.TokenParser
import sprite.parser.utils.TokenParser.{reserved, tokenized}
import sprite.solver.qflia.language.{BinaryRelation, BoolTerm}
import sprite.solver.qflia.parser.BinaryOperationParser.binaryOperation
import sprite.solver.qflia.parser.IntTermParser.baseIntTerm

object BoolTermParser extends BaseTokenParser:

  val truthValue: Parser[BoolTerm] =
    bool.map:
      case true  => BoolTerm.True
      case false => BoolTerm.False

  val binaryRelation: TokenParser[BinaryRelation] =
    Parser.fromStringMap(BinaryRelation.byName).tokenized

  lazy val boolTerm: Parser[BoolTerm] =
    Parser.defer {
      not
        | and
        | or
        | implies
        | iff
        | related
        | boolTerm.between(reserved('('), reserved(')'))
        | truthValue
    }

  lazy val not: Parser[BoolTerm.Not] =
    reserved('!') *> boolTerm.map(BoolTerm.Not.apply)

  lazy val and: Parser[BoolTerm.And] =
    binaryOperation(reserved("&&"), boolTerm, boolTerm).map(BoolTerm.And.apply)

  lazy val or: Parser[BoolTerm.Or] =
    binaryOperation(reserved("||"), boolTerm, boolTerm).map(BoolTerm.Or.apply)

  lazy val implies: Parser[BoolTerm.Implies] =
    binaryOperation(reserved("==>"), boolTerm, boolTerm).map(BoolTerm.Implies.apply)

  lazy val iff: Parser[BoolTerm.Iff] =
    binaryOperation(reserved("<==>"), boolTerm, boolTerm).map(BoolTerm.Iff.apply)

  lazy val related: Parser[BoolTerm.Related] =
    (binaryRelation, baseIntTerm, baseIntTerm).mapN(BoolTerm.Related.apply)

end BoolTermParser
