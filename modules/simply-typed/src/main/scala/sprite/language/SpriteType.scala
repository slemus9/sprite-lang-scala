package sprite.language

enum SpriteType:
  case Base(base: SpriteBaseType)
  case RefinedType(base: SpriteBaseType, refinement: Refinement)
  case FunctionType(paramName: String, paramType: SpriteType, returnType: SpriteType)

enum SpriteBaseType:
  case Bool, Int

final case class Refinement(varName: String, predicate: SpriteTerm)
