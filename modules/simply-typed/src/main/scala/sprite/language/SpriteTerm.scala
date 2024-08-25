package sprite.language

import cats.data.NonEmptyList

enum SpriteTerm:
  case Integer(value: Int)
  case Var(name: String)
  case Let(binding: Bind, body: SpriteTerm)
  case Lambda(params: NonEmptyList[String], body: SpriteTerm)
  case LambdaApply(fun: SpriteTerm, arg: SpriteTerm)

  def isRecursive: Boolean =
    this match
      case _: (Integer | Var) => false
      case _                  => true

final case class Bind(name: String, body: SpriteTerm)
