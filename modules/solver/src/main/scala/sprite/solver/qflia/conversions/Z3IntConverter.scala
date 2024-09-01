package sprite.solver.qflia.conversions

import com.microsoft.z3.*
import sprite.solver.qflia.conversions.Z3ConversionError.InvalidFunction
import sprite.solver.qflia.conversions.Z3ExprConverter.*
import sprite.solver.qflia.language.{ApplyFlattened, IntTerm}
import sprite.solver.qflia.language.IntPrimitive.*
import sprite.solver.qflia.language.IntTerm.*

given Z3IntConverter(using context: Z3Context): Z3ExprConverter[IntTerm, IntSort] with
  override def toZ3(term: IntTerm): Result[Expr[IntSort]] = term match
    case Const(value)         => context.call(_.mkInt(value))
    case Var(name)            => context.lookup(name).map(_.apply())
    case ConstantMul(x, y)    => context.binaryOperation[IntSort](Const(x), y, c => c.mkMul(_, _))
    case apply: ApplyFunction => z3Apply(apply)

  def z3Apply(apply: ApplyFunction): Result[Expr[IntSort]] =
    ApplyFlattened.from(apply) match
      case ApplyFlattened(Var(Add.name), args) =>
        z3Sequence(args.toList).flatMap { args =>
          context.call(_.mkAdd(args*))
        }

      case ApplyFlattened(Var(Sub.name), args) =>
        z3Sequence(args.toList).flatMap { args =>
          context.call(_.mkSub(args*))
        }

      case ApplyFlattened(Var(uninterpreted), args) =>
        for
          f       <- context.lookup(uninterpreted)
          args    <- z3Sequence(args.toList)
          applied <- context.call(_.mkApp(f, args*))
        yield applied

      case ApplyFlattened(term, _) => Left(InvalidFunction(term))
