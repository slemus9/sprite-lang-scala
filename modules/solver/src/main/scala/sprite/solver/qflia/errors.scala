package sprite.solver.qflia

import cats.syntax.either.*
import com.microsoft.z3.Z3Exception
import sprite.solver.qflia.language.IntTerm
import sprite.solver.qflia.language.VerificationCondition.Declaration

import scala.util.control.NoStackTrace

sealed trait VerificationConditionError extends Throwable, NoStackTrace

final class DeclarationNotFound(name: String) extends VerificationConditionError:
  override val getMessage: String =
    s"Could not find the following declaration while interpreting the Verification Condition: $name"

final class ApplyFunctionError(term: IntTerm) extends VerificationConditionError:
  override val getMessage: String =
    s"Cannot use the following term as a function: $term"

final class Z3Error(error: Z3Exception) extends VerificationConditionError:

  override val getCause: Throwable =
    error

  override val getMessage: String =
    "Could not build the Z3 Expr"

object Z3Error:

  def catchException[A](f: => A): Either[Z3Error, A] =
    Either.catchOnly[Z3Exception](f).leftMap(Z3Error(_))
