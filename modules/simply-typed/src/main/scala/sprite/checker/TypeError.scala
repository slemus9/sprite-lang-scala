package sprite.checker

import sprite.language.{SpriteTerm, SpriteType}

import scala.util.control.NoStackTrace

type TypeCheckResult[A] = Either[TypeError, A]

sealed trait TypeError extends Throwable, NoStackTrace

final class TypeNotFound(variable: String) extends TypeError:
  override val getMessage: String =
    s"Variable was not declared: $variable"

final class NonInferable(term: SpriteTerm) extends TypeError:
  override val getMessage: String =
    s"Cannot infer the type of the term: $term"

final class InvalidApply(`type`: SpriteType) extends TypeError:
  override val getMessage: String =
    s"Trying to apply a term that is not a function. Received a term of type: ${`type`}"
