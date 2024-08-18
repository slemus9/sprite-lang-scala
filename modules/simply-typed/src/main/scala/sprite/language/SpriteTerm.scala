package sprite.language

enum SpriteTerm:
  case Integer(value: Int)
  case Bool(value: Boolean)
  case Var(name: String)
  case Let(binding: Bind, body: SpriteTerm)
  case Lambda(param: String, body: SpriteTerm)
  case LambdaApply(fun: SpriteTerm, arg: SpriteTerm)

  def isRecursive: Boolean =
    this match
      case _: (Let | Lambda | LambdaApply) => true
      case _                               => false

final case class Bind(name: String, body: SpriteTerm)
