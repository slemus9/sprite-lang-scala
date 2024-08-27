package sprite.solver.qflia.language

import cats.data.NonEmptyList
import cats.syntax.either.*
import sprite.solver.qflia.ApplyFunctionError

enum IntTerm:
  case Const(value: Int)
  case Var(name: String)
  case ConstantMul(constant: Int, term: IntTerm)
  case ApplyFunction(fun: IntTerm, arg: IntTerm)

  def isRecursive: Boolean =
    this match
      case _: (Const | Var) => false
      case _                => true

final case class FlattenedApply(functionName: String, args: NonEmptyList[IntTerm])

object FlattenedApply:
  import IntTerm.*

  def from(apply: ApplyFunction): Either[ApplyFunctionError, FlattenedApply] =
    build(apply, args = List.empty)

  private def build(
      apply: ApplyFunction,
      args: List[IntTerm]
  ): Either[ApplyFunctionError, FlattenedApply] =
    apply match
      case ApplyFunction(Var(name), arg)            => Right(FlattenedApply(name, NonEmptyList(arg, args)))
      case ApplyFunction(apply: ApplyFunction, arg) => build(apply, arg :: args)
      case ApplyFunction(term, _)                   => Left(ApplyFunctionError(term))
