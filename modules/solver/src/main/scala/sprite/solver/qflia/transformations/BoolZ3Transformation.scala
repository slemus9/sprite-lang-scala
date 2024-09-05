package sprite.solver.qflia.transformations

import com.microsoft.z3.{BoolSort, Context, Expr, IntSort}
import sprite.solver.qflia.language.{BoolTerm, IntTerm}
import sprite.solver.qflia.language.BinaryRelation
import sprite.solver.qflia.language.BinaryRelation.*
import sprite.solver.qflia.language.BoolTerm.*
import sprite.solver.qflia.transformations.Z3Transformation.Result

given BoolZ3Transformation(using context: Z3Context): Z3Transformation[BoolTerm, BoolSort] with

  override def convert(term: BoolTerm): Result[Expr[BoolSort]] = term match
    case True                      => context.call(_.mkTrue)
    case False                     => context.call(_.mkFalse)
    case Not(p)                    => p.transform[Result].into.flatMap(b => context.call(_.mkNot(b)))
    case And(p1, p2)               => context.binaryOperation[BoolSort](p1, p2, c => c.mkAnd(_, _))
    case Or(p1, p2)                => context.binaryOperation[BoolSort](p1, p2, c => c.mkOr(_, _))
    case Implies(p1, p2)           => context.binaryOperation[BoolSort](p1, p2, _.mkImplies)
    case Iff(p1, p2)               => context.binaryOperation[BoolSort](p1, p2, _.mkIff)
    case Related(relation, t1, t2) => context.binaryOperation[IntSort](t1, t2, z3Relation(relation))

  def z3Relation(
      relation: BinaryRelation
  ): Context => (Expr[IntSort], Expr[IntSort]) => Expr[BoolSort] = relation match
    case Equal       => _.mkEq
    case GreaterOrEq => _.mkGe
    case LessOrEq    => _.mkLe
    case Greater     => _.mkGt
    case Less        => _.mkLt
