package sprite.solver.qflia.substitutions

import sprite.languages.Substitution
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.language.IntTerm.*

given IntTermSubstitution: Substitution[IntTerm, IntTerm] with

  override def substitute(term: IntTerm, variable: String, body: IntTerm): IntTerm = term match
    case c: Const                      => c
    case Var(name) if name == variable => term
    case v: Var                        => v
    case ConstantMul(c, x)             => ConstantMul(c, substitute(x, variable, body))
    case ApplyFunction(fun, arg)       =>
      ApplyFunction(
        substitute(fun, variable, body),
        substitute(arg, variable, body)
      )
