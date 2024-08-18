package sprite.language

enum SpriteDeclaration:
  case SuperCombinator(name: String, params: List[String], term: SpriteTerm)
  case SuperCombinatorType(name: String, spriteType: SpriteType)
