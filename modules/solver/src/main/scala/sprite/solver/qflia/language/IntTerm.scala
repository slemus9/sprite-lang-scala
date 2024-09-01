package sprite.solver.qflia.language

import cats.data.NonEmptyList
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

  def substitute(variable: String, term: IntTerm): IntTerm = this match
    case c: Const                      => c
    case Var(name) if name == variable => term
    case v: Var                        => v
    case ConstantMul(c, x)             => ConstantMul(c, x.substitute(variable, term))
    case ApplyFunction(fun, arg)       =>
      ApplyFunction(
        fun.substitute(variable, term),
        arg.substitute(variable, term)
      )

final case class ApplyFlattened(fun: IntTerm, args: NonEmptyList[IntTerm])

object ApplyFlattened:
  import IntTerm.ApplyFunction

  def from(apply: ApplyFunction): ApplyFlattened =
    build(apply, args = List.empty)

  private def build(apply: ApplyFunction, args: List[IntTerm]): ApplyFlattened =
    apply match
      case ApplyFunction(apply: ApplyFunction, arg) => build(apply, arg :: args)
      case ApplyFunction(term, arg)                 => ApplyFlattened(term, NonEmptyList(arg, args))
