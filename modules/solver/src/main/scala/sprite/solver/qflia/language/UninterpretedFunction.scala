package sprite.solver.qflia.language

import com.microsoft.z3.*

final case class UninterpretedFunction[R <: Sort](
    name: String,
    domain: Sort,
    range: R
)(using context: Context):

  val toZ3: FuncDecl[R] =
    context.mkFuncDecl(name, domain, range)
