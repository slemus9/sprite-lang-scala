package sprite.solver.qflia

import cats.effect.Sync
import com.microsoft.z3
import sprite.solver.qflia.language.{Declaration, Sort}

final class QF_LIA_Solver[F[_]] private (
    private val context: z3.Context,
    private val z3Solver: z3.Solver
)(using F: Sync[F]):

  private val push: F[Unit] =
    F.delay(z3Solver.push)

  private val pop: F[Unit] =
    F.delay(z3Solver.pop)

  def declareConst(const: Declaration.Const): F[Unit] =
    ???

  def z3Sort(sort: Sort): z3.Sort = sort match
    case Sort.Bool => context.getBoolSort
    case Sort.Int  => context.getIntSort

object QF_LIA_Solver:

  def apply[F[_]: Sync](context: z3.Context): QF_LIA_Solver[F] =
    val params = context.mkParams
    val solver = context.mkSolver
    params.add("logic", "QF_LIA")
    solver.setParameters(params)

    new QF_LIA_Solver(context, solver)

  // extension (solver: QF_LIA_Solver)
  //   def isValid(context: Context, predicate: Predicate): Boolean =
  //     given Context = context
  //     solver.check(Predicate.Not(predicate).toZ3) match
  //       case Status.UNSATISFIABLE => true
  //       case _                    => false
