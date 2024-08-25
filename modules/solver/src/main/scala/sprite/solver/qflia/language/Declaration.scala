package sprite.solver.qflia.language

enum Declaration:
  case Const[S <: Sort](name: String, sort: S)
  case Function[Domain <: Sort, Range <: Sort](name: String, domain: Domain, range: Range)
