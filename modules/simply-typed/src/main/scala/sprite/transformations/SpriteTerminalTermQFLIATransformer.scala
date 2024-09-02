package sprite.transformations

import cats.Id
import sprite.language.SpriteTerm
import sprite.language.SpriteTerm.TerminalTerm
import sprite.languages.Transformation
import sprite.solver.qflia.language.IntTerm

given SpriteTerminalTermQFLIATransformer: Transformation[Id, TerminalTerm, IntTerm] with
  override def transform(from: TerminalTerm): IntTerm = from match
    case SpriteTerm.IntConst(value) => IntTerm.Const(value)
    case SpriteTerm.Var(name)       => IntTerm.Var(name)
