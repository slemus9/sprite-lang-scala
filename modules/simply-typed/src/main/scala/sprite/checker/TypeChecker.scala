package sprite.checker

import sprite.language.*
import sprite.language.SpriteTerm.*
import sprite.solver.qflia.language.Constraint

trait TypeChecker:
  synthetizer: TypeSynthetizer =>

  def check(
      context: Map[String, SpriteType],
      term: SpriteTerm,
      expectedType: SpriteType
  ): TypeCheckResult[Constraint] = term match
    case Lambda(params, body)            => ???
    case Let(binding, body)              => ???
    case term: TypeSynthetizer.Inferable => ???

object TypeChecker
