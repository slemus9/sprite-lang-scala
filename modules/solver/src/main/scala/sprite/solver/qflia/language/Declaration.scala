package sprite.solver.qflia.language

enum Declaration:
  case Const(name: String, sort: Sort)
  case Function(name: String, domain: Sort, range: Sort)
  case Assertion(bool: BoolTerm)
