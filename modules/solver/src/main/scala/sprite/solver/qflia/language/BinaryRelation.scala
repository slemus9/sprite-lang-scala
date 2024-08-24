package sprite.solver.qflia.language

import com.microsoft.z3.*

enum BinaryRelation:
  case Equal,
    GreaterOrEq,
    LessOrEq,
    Greater,
    Less

  def toZ3(using context: Context): (Expr[IntSort], Expr[IntSort]) => BoolExpr =
    this match
      case Equal       => context.mkEq
      case GreaterOrEq => context.mkGe
      case LessOrEq    => context.mkLe
      case Greater     => context.mkGt
      case Less        => context.mkLt
