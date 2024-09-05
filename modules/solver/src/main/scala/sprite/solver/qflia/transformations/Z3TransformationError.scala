package sprite.solver.qflia.transformations

import com.microsoft.z3.Z3Exception
import sprite.solver.qflia.language.IntTerm

import scala.util.control.NoStackTrace

sealed trait Z3TransformationError extends Throwable, NoStackTrace

object Z3TransformationError:

  final class DeclarationNotFound(name: String) extends Z3TransformationError:
    override val getMessage: String =
      s"Could not find declaration in the environment: $name"

  final class InvalidFunction(term: IntTerm) extends Z3TransformationError:
    override val getMessage: String =
      s"Cannot use the term as a function: $term"

  final class Z3Error(t: Z3Exception) extends Z3TransformationError:
    override val getCause: Throwable = t.getCause
    override val getMessage: String  = t.getMessage
