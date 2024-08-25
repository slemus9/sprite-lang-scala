package sprite.language

import sprite.solver.qflia.language.BoolTerm

enum SpriteType:
  case Base(base: SpriteBaseType)
  case RefinedType(base: SpriteBaseType, refinement: Refinement)
  case FunctionType(param: String, paramType: SpriteType, returnType: SpriteType)

enum SpriteBaseType:
  case Int

final case class Refinement(param: String, predicate: BoolTerm)
