package sprite.solver.qflia.language

enum BoolTerm:
  case True, False
  case Not(p: BoolTerm)
  case And(p1: BoolTerm, p2: BoolTerm)
  case Or(p1: BoolTerm, p2: BoolTerm)
  case Implies(p1: BoolTerm, p2: BoolTerm)
  case Iff(p1: BoolTerm, p2: BoolTerm)
  case Related(relation: BinaryRelation, t1: IntTerm, t2: IntTerm)

  def isRecursive: Boolean =
    this match
      case True | False => false
      case _            => true
