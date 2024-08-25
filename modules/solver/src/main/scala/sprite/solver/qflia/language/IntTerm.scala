package sprite.solver.qflia.language

import cats.data.NonEmptyList

enum IntTerm:
  case Const(value: Int)
  case Var(name: String)
  case ConstantMul(constant: Int, term: IntTerm)
  case ApplyFunction(fun: IntTerm, arg: IntTerm)

  def isRecursive: Boolean =
    this match
      case _: (Const | Var) => false
      case _                => true
