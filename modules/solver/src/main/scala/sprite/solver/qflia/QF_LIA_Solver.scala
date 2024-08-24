package sprite.solver.qflia

import com.microsoft.z3.{Context, Solver, Status}
import sprite.solver.qflia.language.Predicate

opaque type QF_LIA_Solver <: Solver = Solver

object QF_LIA_Solver:

  def from(context: Context): QF_LIA_Solver =
    val params = context.mkParams
    val solver = context.mkSolver
    params.add("logic", "QF_LIA")
    solver.setParameters(params)

    solver

  extension (solver: QF_LIA_Solver)
    def isValid(context: Context, predicate: Predicate): Boolean =
      given Context = context
      solver.check(Predicate.Not(predicate).toZ3) match
        case Status.UNSATISFIABLE => true
        case _                    => false
