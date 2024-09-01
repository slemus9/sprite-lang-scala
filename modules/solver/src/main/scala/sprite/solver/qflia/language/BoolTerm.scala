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

  def substitute(variable: String, term: IntTerm): BoolTerm = this match
    case True                      => True
    case False                     => False
    case Not(p)                    => Not(p.substitute(variable, term))
    case And(p1, p2)               => And(p1.substitute(variable, term), p2.substitute(variable, term))
    case Or(p1, p2)                => Or(p1.substitute(variable, term), p2.substitute(variable, term))
    case Implies(p1, p2)           => Implies(p1.substitute(variable, term), p2.substitute(variable, term))
    case Iff(p1, p2)               => Iff(p1.substitute(variable, term), p2.substitute(variable, term))
    case Related(relation, t1, t2) =>
      Related(
        relation,
        t1.substitute(variable, term),
        t2.substitute(variable, term)
      )
