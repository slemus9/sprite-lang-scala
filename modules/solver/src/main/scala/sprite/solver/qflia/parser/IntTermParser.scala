package sprite.solver.qflia.parser

import cats.parse.Parser
import cats.syntax.all.*
import sprite.parser.utils.{BaseTokenParser, TokenParser}
import sprite.parser.utils.TokenParser.{reserved, tokenized}
import sprite.solver.qflia.language.IntPrimitive
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.parser.BinaryOperationParser.binaryOperation

object IntTermParser extends BaseTokenParser:

  val const: Parser[IntTerm] =
    positiveInt.map(IntTerm.Const.apply)

  val naryOperators: TokenParser[IntPrimitive] =
    Parser.fromStringMap(IntPrimitive.naryByName).tokenized

  val variable: TokenParser[IntTerm] =
    identifier
      .orElse(naryOperators.map(_.name))
      .map(IntTerm.Var.apply)
      .tokenized

  lazy val intTerm: Parser[IntTerm] =
    Parser.defer {
      constMul
        | applyFunction
        | baseIntTerm
    }

  lazy val baseIntTerm: Parser[IntTerm] =
    const
      | variable
      | intTerm.between(reserved('('), reserved(')'))

  lazy val constMul: Parser[IntTerm] =
    reserved(IntPrimitive.Mul.name) *> (
      (positiveInt ~ baseIntTerm).map(IntTerm.ConstantMul.apply) | (baseIntTerm ~ positiveInt).map(
        (term, constant) => IntTerm.ConstantMul(constant, term)
      )
    )

  lazy val applyFunction: Parser[IntTerm] =
    baseIntTerm.rep.map { terms =>
      terms.reduceLeft(IntTerm.ApplyFunction.apply)
    }
