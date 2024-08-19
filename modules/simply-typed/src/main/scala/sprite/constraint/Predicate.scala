package sprite.constraint

enum Predicate:
  case True, False
  case Var(name: String)
  case Not(p: Predicate)
  case And(p1: Predicate, p2: Predicate)
  case Or(p1: Predicate, p2: Predicate)
  case UninterpFunction(name: String, arg: Predicate)
