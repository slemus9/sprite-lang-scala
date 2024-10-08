package sprite.checker

import sprite.language.{SpriteTerm, SpriteType}
import sprite.printer.SpritePrinter

import scala.util.control.NoStackTrace

type TypeCheckResult[A] = Either[TypeError, A]

sealed trait TypeError extends Throwable, NoStackTrace

final class UndeclaredVariable(variable: String) extends TypeError:
  override val getMessage: String =
    s"Variable was not declared: $variable"

final class NonInferable(term: SpriteTerm) extends TypeError:
  override val getMessage: String =
    s"Cannot infer the type of the term: $term"

final class InvalidApply(`type`: SpriteType) extends TypeError:
  override val getMessage: String =
    s"Trying to apply a term that is not a function. Received a term of type: ${`type`}"

final class InvalidSubtypingRelation(t1: SpriteType, t2: SpriteType) extends TypeError:
  override def getMessage(): String =
    s"Type ${SpritePrinter.printType(t1)} is not a subtype of ${SpritePrinter.printType(t2)}"
