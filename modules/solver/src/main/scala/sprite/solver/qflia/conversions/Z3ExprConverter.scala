package sprite.solver.qflia.conversions

import cats.syntax.traverse.*
import com.microsoft.z3.{Expr, Sort}
import sprite.solver.qflia.conversions.Z3ConversionError.*
import sprite.solver.qflia.conversions.Z3ExprConverter.Result

trait Z3ExprConverter[A, S <: Sort]:

  def toZ3(term: A): Result[Expr[S]]

object Z3ExprConverter:

  type Result[A] = Either[Z3ConversionError, A]

  def z3Sequence[A, S <: Sort](terms: List[A])(using Z3ExprConverter[A, S]): Result[List[Expr[S]]] =
    terms.traverse(_.toZ3)

  extension [A](x: A)
    def toZ3[S <: Sort](using converter: Z3ExprConverter[A, S]): Result[Expr[S]] =
      converter.toZ3(x)
