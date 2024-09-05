package sprite.checker

import sprite.checker.TypeSynthetizer.Inferable
import sprite.language.*
import sprite.language.SpriteTerm.*
import sprite.language.SpriteType.RefinedType
import sprite.languages.Substitution.substitute
import sprite.solver.qflia.language.Constraint
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.substitutions.BoolTermSubstitution

trait TypeChecker:
  synthetizer: TypeSynthetizer =>

  def check(
      context: Map[String, SpriteType],
      term: SpriteTerm,
      expectedType: SpriteType
  ): TypeCheckResult[Constraint] = term match
    case Lambda(params, body) => ???

    case Let(binding, body) => ???

    case term: Inferable => ???

  def implicationConstraint(
      param: String,
      paramType: SpriteType,
      consequent: Constraint
  ): Constraint = paramType match
    case RefinedType(b, Refinement(v, p)) =>
      Constraint.forall(
        param,
        antecedent = p.substitute(v, IntTerm.Var(param)),
        consequent
      )

    case _ => consequent

object TypeChecker
