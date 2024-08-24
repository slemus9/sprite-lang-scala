package sprite.solver.qflia.language

import com.microsoft.z3.*

/** Subset of the language accepted by Z3 that corresponds to the Quantifier-free fragment of linear
  * arithmetic and uninterpreted functions
  */
enum Predicate:
  case Related(relation: BinaryRelation, t1: IntTerm, t2: IntTerm)
  case ApplyPredicate(fun: UninterpretedFunction[BoolSort], args: List[IntTerm])
  case And(p1: Predicate, p2: Predicate)
  case Or(p1: Predicate, p2: Predicate)
  case Implies(p1: Predicate, p2: Predicate)
  case Iff(p1: Predicate, p2: Predicate)
  case Not(p: Predicate)
  case True, False

  def toZ3(using context: Context): Expr[BoolSort] =
    this match
      case Related(relation, t1, t2) => relation.toZ3(t1.toZ3, t2.toZ3)
      case ApplyPredicate(fun, args) => context.mkApp(fun.toZ3, args.map(_.toZ3)*)
      case And(p1, p2)               => context.mkAnd(p1.toZ3, p2.toZ3)
      case Or(p1, p2)                => context.mkOr(p1.toZ3, p2.toZ3)
      case Implies(p1, p2)           => context.mkImplies(p1.toZ3, p2.toZ3)
      case Iff(p1, p2)               => context.mkIff(p1.toZ3, p2.toZ3)
      case Not(p)                    => context.mkNot(p.toZ3)
      case True                      => context.mkTrue
      case False                     => context.mkFalse
