package sprite.language

import cats.data.NonEmptyList

enum SpriteTerm:
  case IntConst(value: Int)
  case Var(name: String)
  case Let(binding: Bind, body: SpriteTerm)
  case Lambda(params: NonEmptyList[String], body: SpriteTerm)
  case LambdaApply(fun: SpriteTerm, arg: SpriteTerm.TerminalTerm)
  case Annotation(term: SpriteTerm, annotated: SpriteType)

  def isRecursive: Boolean =
    this match
      case _: (IntConst | Var) => false
      case _                   => true

final case class Bind(name: String, body: SpriteTerm)

object SpriteTerm:

  type TerminalTerm = IntConst | Var
