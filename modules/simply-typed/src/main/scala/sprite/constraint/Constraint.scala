package sprite.constraint

import sprite.language.SpriteBaseType

enum Constraint:
  case Single(p: Predicate)
  case And(c1: Constraint, c2: Constraint)
  case Forall(param: String, paramType: SpriteBaseType, body: Implication)

final case class Implication(p: Predicate, c: Constraint)
