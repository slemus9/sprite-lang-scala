package sprite.solver.qflia.conversions

import cats.syntax.all.*
import com.microsoft.z3.*
import sprite.solver.qflia.conversions.Z3ConversionError.*
import sprite.solver.qflia.conversions.Z3ExprConverter.*
import sprite.solver.qflia.language.VerificationCondition.Declaration

final case class Z3Context(
    environment: Map[String, FuncDecl[IntSort]],
    z3: Context
):
  def lookup(name: String): Either[DeclarationNotFound, FuncDecl[IntSort]] =
    environment.get(name).toRight(DeclarationNotFound(name))

  def call[A](unsafe: Context => A): Either[Z3Error, A] =
    Z3Context.wrapZ3(unsafe(z3))

  def binaryOperation[S <: Sort]: BinaryOperation[S] =
    new BinaryOperation[S]

  final class BinaryOperation[S1 <: Sort]:
    def apply[A, S2 <: Sort](
        term1: A,
        term2: A,
        makeOperation: Context => (Expr[S1], Expr[S1]) => Expr[S2]
    )(using Z3ExprConverter[A, S1]): Result[Expr[S2]] =
      (term1.toZ3, term2.toZ3).flatMapN { (x, y) =>
        call(makeOperation).map(_.apply(x, y))
      }

object Z3Context:

  def build(declarations: List[Declaration])(using context: Context): Either[Z3Error, Z3Context] =
    val intSort = context.getIntSort
    declarations
      .traverse {
        case Declaration.Const(name) =>
          wrapZ3(context.mkConstDecl(name, intSort)).map(name -> _)

        case Declaration.Function(name, arity) =>
          val domain = Array.fill[Sort](arity)(intSort)
          wrapZ3(context.mkFuncDecl(name, domain, intSort)).map(name -> _)
      }
      .map { entries =>
        Z3Context(environment = entries.toMap, z3 = context)
      }

  private def wrapZ3[A](unsafe: => A): Either[Z3Error, A] =
    Either.catchOnly[Z3Exception](unsafe).leftMap(Z3Error(_))
