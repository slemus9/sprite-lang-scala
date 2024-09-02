package sprite.checker

import cats.syntax.all.*
import sprite.checker.TypeSynthetizer.*
import sprite.language.{SpriteTerm, SpriteType}
import sprite.language.SpriteTerm.*
import sprite.language.SpriteType.FunctionType
import sprite.languages.Substitution.substitute
import sprite.languages.Transformation.to
import sprite.solver.qflia.language.{Constraint, IntTerm}
import sprite.solver.qflia.parser.IntTermParser.variable
import sprite.substitutions.SpriteTypeSubstitution
import sprite.transformations.SpriteTerminalTermQFLIATransformer

trait TypeSynthetizer:
  checker: TypeChecker =>

  def synth(context: Map[String, SpriteType], term: Inferable): TypeCheckResult[Synth] = term match
    case x: IntConst =>
      Synth(constraint = Constraint.alwaysHolds, inferred = SpriteType.primitive(x)).pure

    case Var(name) =>
      context.get(name).toRight(UndeclaredVariable(name)).map { t =>
        Synth(constraint = Constraint.alwaysHolds, inferred = t)
      }

    case Annotation(term, t) =>
      checker.check(context, term, expectedType = t).map { constraint =>
        Synth(constraint, inferred = t)
      }

    case LambdaApply(fun: Inferable, arg) =>
      synth(context, fun).flatMap {
        case Synth(funConstraint, funType: FunctionType) =>
          synthLambdaApply(context, funConstraint, funType, argument = arg)

        case other =>
          InvalidApply(other.inferred).raiseError
      }

    case LambdaApply(fun, _) =>
      NonInferable(fun).raiseError

  private def synthLambdaApply(
      context: Map[String, SpriteType],
      funConstraint: Constraint,
      funType: FunctionType,
      argument: SpriteTerm.TerminalTerm
  ): TypeCheckResult[Synth] =
    checker.check(context, argument, expectedType = funType.paramType).map { argConstraint =>
      val inferred = funType.returnType.substitute(
        variable = funType.param,
        body = argument.to[IntTerm]
      )

      Synth(constraint = funConstraint and argConstraint, inferred)
    }

end TypeSynthetizer

object TypeSynthetizer:

  type Inferable = IntConst | Var | LambdaApply | Annotation

  final case class Synth(
      constraint: Constraint,
      inferred: SpriteType
  )
