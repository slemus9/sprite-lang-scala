package sprite.solver.qflia.transformations

import cats.syntax.all.*
import com.microsoft.z3.*
import sprite.solver.qflia.transformations.Z3Transformation.Result
import sprite.solver.qflia.transformations.Z3TransformationError.*

import scala.collection.immutable.HashMap

final case class Z3Context(
    environment: HashMap[String, FuncDecl[IntSort]],
    z3: Context
):
  def addDeclaration(name: String, declaration: FuncDecl[IntSort]): Z3Context =
    copy(environment = environment + (name -> declaration))

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
    )(using Z3Transformation[A, S1]): Result[Expr[S2]] =
      (term1.transform.into, term2.transform.into).flatMapN { (x, y) =>
        call(makeOperation).map(_.apply(x, y))
      }

object Z3Context:

  def empty(z3: Context) = Z3Context(
    environment = HashMap.empty,
    z3
  )

  private def wrapZ3[A](unsafe: => A): Either[Z3Error, A] =
    Either.catchOnly[Z3Exception](unsafe).leftMap(Z3Error(_))
