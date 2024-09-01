package sprite.language

enum SpriteDeclaration:
  case TermDeclaration(name: String, term: SpriteTerm)
  case TypeDeclaration(name: String, spriteType: SpriteType)
