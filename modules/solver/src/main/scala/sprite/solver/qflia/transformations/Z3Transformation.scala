package sprite.solver.qflia.transformations

import com.microsoft.z3.{Expr, Sort}
import sprite.languages.Transformation
import sprite.solver.qflia.transformations.Z3Transformation.Result

type Z3Transformation[A, S <: Sort] = Transformation[Result, A, Expr[S]]

object Z3Transformation:

  type Result[A] = Either[Z3TransformationError, A]
