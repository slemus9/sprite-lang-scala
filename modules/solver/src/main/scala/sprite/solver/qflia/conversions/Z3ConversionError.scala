package sprite.solver.qflia.conversions

import com.microsoft.z3.Z3Exception
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.language.VerificationCondition.Declaration

import scala.util.control.NoStackTrace

sealed trait Z3ConversionError extends Throwable, NoStackTrace

object Z3ConversionError:

  final class DeclarationNotFound(name: String) extends Z3ConversionError:
    override val getMessage: String =
      s"Could not find declaration in the environment: $name"

  final class InvalidFunction(term: IntTerm) extends Z3ConversionError:
    override val getMessage: String =
      s"Cannot use the term as a function: $term"

  final class Z3Error(t: Z3Exception) extends Z3ConversionError:
    override val getCause: Throwable = t.getCause
    override val getMessage: String  = t.getMessage
