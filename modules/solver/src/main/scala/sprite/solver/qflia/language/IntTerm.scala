package sprite.solver.qflia.language

import com.microsoft.z3.*

enum IntTerm:
  case Const(value: Int)
  case Var(name: String)
  case Add(t1: IntTerm, t2: IntTerm)
  case Sub(t1: IntTerm, t2: IntTerm)
  case ConstantMul(constant: Int, term: IntTerm)
  case ApplyFunction(fun: UninterpretedFunction[IntSort], args: List[IntTerm])

  def toZ3(using context: Context): Expr[IntSort] =
    this match
      case Const(value)                => context.mkInt(value)
      case Var(name)                   => context.mkIntConst(name)
      case Add(t1, t2)                 => context.mkAdd(t1.toZ3, t2.toZ3)
      case Sub(t1, t2)                 => context.mkSub(t1.toZ3, t2.toZ3)
      case ConstantMul(constant, term) => context.mkMul(context.mkInt(constant), term.toZ3)
      case ApplyFunction(fun, args)    => context.mkApp(fun.toZ3, args.map(_.toZ3)*)
