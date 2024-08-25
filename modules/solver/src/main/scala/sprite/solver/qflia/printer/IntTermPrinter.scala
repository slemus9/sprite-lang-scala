package sprite.solver.qflia.printer

import cats.data.NonEmptyList
import sprite.solver.qflia.language.IntTerm

object IntTermPrinter:

  def printTerm(term: IntTerm): String = term match
    case IntTerm.Const(value)                => value.toString
    case IntTerm.Var(name)                   => name
    case IntTerm.ConstantMul(constant, term) => s"* $constant ${printWithParensIfRec(term)}"
    case apply: IntTerm.ApplyFunction        => printApply(apply)

  def printApply(apply: IntTerm.ApplyFunction): String = apply match
    case IntTerm.ApplyFunction(apply: IntTerm.ApplyFunction, arg) =>
      s"${printApply(apply)} ${printWithParensIfRec(arg)}"

    case IntTerm.ApplyFunction(fun, arg) =>
      s"${printWithParensIfRec(fun)} ${printWithParensIfRec(arg)}"

  def printWithParensIfRec(term: IntTerm) =
    if term.isRecursive then s"(${printTerm(term)})" else printTerm(term)
