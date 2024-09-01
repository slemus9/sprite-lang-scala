package sprite.checker

import cats.syntax.all.*
import sprite.checker.TypeSynthetizer.*
import sprite.language.{SpriteTerm, SpriteType}
import sprite.language.SpriteTerm.*
import sprite.language.SpriteType.FunctionType
import sprite.solver.qflia.language.Constraint

trait TypeSynthetizer:
  checker: TypeChecker =>

  def synth(context: Map[String, SpriteType], term: Inferable): TypeCheckResult[Synth] = term match
    case x: Integer =>
      Synth(constraint = Constraint.alwaysHolds, inferred = SpriteType.primitive(x)).pure

    case Var(name) =>
      context.get(name).toRight(TypeNotFound(name)).map { t =>
        Synth(constraint = Constraint.alwaysHolds, inferred = t)
      }

    case Annotation(term, t) =>
      checker.check(context, term, expectedType = t).map { constraint =>
        Synth(constraint, inferred = t)
      }

    case LambdaApply(fun: Inferable, arg) =>
      synth(context, fun).flatMap {
        case Synth(c1, FunctionType(param, paramType, returnType)) =>
          checker.check(context, arg, expectedType = paramType).map { c2 =>
            Synth(
              constraint = c1 and c2,
              inferred = returnType // TODO: substitution
            )
          }

        case other =>
          InvalidApply(other.inferred).raiseError
      }

    case LambdaApply(fun, _) =>
      NonInferable(fun).raiseError

object TypeSynthetizer:

  type Inferable = Integer | Var | LambdaApply | Annotation

  final case class Synth(
      constraint: Constraint,
      inferred: SpriteType
  )
