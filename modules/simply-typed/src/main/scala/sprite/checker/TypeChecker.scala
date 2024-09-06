package sprite.checker

import cats.syntax.either.*
import sprite.language.*
import sprite.language.SpriteTerm.*
import sprite.language.SpriteType.*
import sprite.languages.Substitution.substitute
import sprite.solver.qflia.language.Constraint
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.substitutions.BoolTermSubstitution
import sprite.substitutions.SpriteTypeSubstitution
import sprite.Example

trait TypeChecker:
  synthetizer: TypeSynthetizer =>

  def check(
      context: Map[String, SpriteType],
      term: SpriteTerm,
      expectedType: SpriteType
  ): TypeCheckResult[Constraint] = (term, expectedType) match
    case (Lambda(params, body), funType: FunctionType) =>
      val assumptions = FlattenedFunctionType.from(funType).params.toVector.take(params.size)
      check(
        context = context ++ assumptions,
        term = body,
        expectedType = funType.returnType
      ).map { constraint =>
        assumptions.foldRight(constraint) { case ((param, paramType), constraint) =>
          implicationConstraint(param, paramType, constraint)
        }
      }

    case (Let(Bind(x, e1), e2), t2) =>
      for
        e1Synth  <- synthetizer.synth(context, e1)
        e2Constr <- check(
                      context = context + (x -> e1Synth.inferred),
                      term = e2,
                      expectedType = t2
                    )
      yield e1Synth.constraint and implicationConstraint(
        param = x,
        paramType = e1Synth.inferred,
        consequent = e2Constr
      )

    case (term, t) =>
      for
        termSynth <- synthetizer.synth(context, term)
        isSubtype <- subtypingConstraint(termSynth.inferred, t)
      yield termSynth.constraint and isSubtype

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

  def subtypingConstraint(
      t1: SpriteType,
      t2: SpriteType
  ): TypeCheckResult[Constraint] = (t1, t2).match
    case (_, Base(_)) =>
      Constraint.alwaysHolds.asRight

    case (RefinedType(b1, Refinement(v1, p1)), RefinedType(b2, Refinement(v2, p2))) =>
      val consequent = Constraint.of(
        p2.substitute(v2, IntTerm.Var(v1))
      )

      Constraint.forall(param = v1, antecedent = p1, consequent).asRight

    case (FunctionType(x1, s1, t1), FunctionType(x2, s2, t2)) =>
      for
        isContravariant <- subtypingConstraint(s2, s1)
        isCovariant     <- subtypingConstraint(t1.substitute(x1, IntTerm.Var(x2)), t2)
      yield isContravariant and implicationConstraint(
        param = x2,
        paramType = s2,
        consequent = isCovariant
      )

    case _ =>
      InvalidSubtypingRelation(t1, t2).asLeft

end TypeChecker

object TypeChecker:

  object instance extends TypeChecker, TypeSynthetizer
