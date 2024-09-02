package sprite.substitutions

import sprite.language.*
import sprite.language.SpriteType.*
import sprite.languages.Substitution
import sprite.languages.Substitution.substitute
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.substitutions.BoolTermSubstitution

given SpriteTypeSubstitution: Substitution[SpriteType, IntTerm] with
  override def substitute(term: SpriteType, variable: String, body: IntTerm): SpriteType =
    term match
      case b: Base => b

      case r @ RefinedType(_, Refinement(v, _)) if v == variable => r

      case RefinedType(b, Refinement(v, p)) =>
        RefinedType(b, Refinement(v, p.substitute(variable, body)))

      case FunctionType(x, s, t) if x == variable =>
        FunctionType(x, s.substitute(variable, body), t)

      case FunctionType(x, s, t) =>
        FunctionType(x, s.substitute(variable, body), t.substitute(variable, body))
