package sprite.solver.qflia.transformations

import com.microsoft.z3.{Expr, IntSort}
import sprite.solver.qflia.language.{ApplyFlattened, IntTerm}
import sprite.solver.qflia.language.IntPrimitive.*
import sprite.solver.qflia.language.IntTerm.*
import sprite.solver.qflia.transformations.Z3Transformation.Result
import sprite.solver.qflia.transformations.Z3TransformationError.InvalidFunction

given IntZ3Transformation(using context: Z3Context): Z3Transformation[IntTerm, IntSort] with

  override def convert(term: IntTerm): Result[Expr[IntSort]] = term match
    case Const(value)         => context.call(_.mkInt(value))
    case Var(name)            => context.lookup(name).map(_.apply())
    case ConstantMul(x, y)    => context.binaryOperation[IntSort](Const(x), y, c => c.mkMul(_, _))
    case apply: ApplyFunction => z3Apply(apply)

  def z3Apply(apply: ApplyFunction): Result[Expr[IntSort]] =
    ApplyFlattened.from(apply) match
      case ApplyFlattened(Var(Add.name), args) =>
        args.toList.transformMany[Result].into[Expr[IntSort]].flatMap { args =>
          context.call(_.mkAdd(args*))
        }

      case ApplyFlattened(Var(Sub.name), args) =>
        args.toList.transformMany[Result].into[Expr[IntSort]].flatMap { args =>
          context.call(_.mkSub(args*))
        }

      case ApplyFlattened(Var(uninterpreted), args) =>
        for
          f       <- context.lookup(uninterpreted)
          args    <- args.toList.transformMany[Result].into[Expr[IntSort]]
          applied <- context.call(_.mkApp(f, args*))
        yield applied

      case ApplyFlattened(term, _) => Left(InvalidFunction(term))
