package sprite

import cats.effect.{IO, IOApp, Resource}
import com.microsoft.z3.*

object Z3Example extends IOApp.Simple:

  override def run: IO[Unit] =
    z3ContextResource.use: context =>

      val params = context.mkParams()
      val solver = context.mkSolver()
      params.add("logic", "QF_LIA")
      solver.setParameters(params)

      exampleSolving2(context, solver)

  def z3ContextResource: Resource[IO, Context] =
    Resource.fromAutoCloseable(IO.blocking(Context()))

  def exampleSolving2(context: Context, solver: Solver): IO[Unit] =
    given Context = context
    val x         = context.mkIntConst("x")
    val xPred     = context.mkLe(0, x)
    val y         = context.mkIntConst("y")
    val yPred     = context.mkEq(y, context.mkAdd(x, 1))
    val postPred  = context.mkLe(0, y)
    val typeSpec  = s"x:Int{${xPred.getSExpr}} -> y:Int{${yPred.getSExpr}} -> Int{${postPred}}"

    for
      _      <- IO.delay(solver.push)
      _      <- IO.println(s"Checking Type: $typeSpec")
      _      <- IO.delay(solver.add(xPred, yPred, context.mkNot(postPred)))
      status <- IO.delay(solver.check)
      _      <- IO.delay(solver.pop)
      _      <- IO.println(s"Result: $status")
      _      <- IO.println(s"Type checks? ${status == Status.UNSATISFIABLE}")
    yield ()

  def exampleSolving1(context: Context, solver: Solver): IO[Unit] =
    for
      _        <- IO.delay(solver.push)
      example1 <- IO.delay(examplePredicate(context))
      _        <- IO.println(example1.getSExpr)
      _        <- IO.delay(solver.add(example1))
      isSat    <- IO.delay(solver.check)
      _        <- IO.println(s"Is Satisfiable : $isSat")
      _        <- IO.println(solver.getModel)
      _        <- IO.println("----------------------")
      _        <- IO.delay(solver.pop)
      _        <- IO.delay(solver.push)
      example2 <- IO.delay(context.mkNot(example1))
      _        <- IO.println(example2.getSExpr)
      _        <- IO.delay(solver.add(example2))
      isSat    <- IO.delay(solver.check)
      _        <- IO.println(s"Result: $isSat")
      _        <- IO.delay(solver.pop)
    yield ()

  def examplePredicate(context: Context): BoolExpr =
    given Context = context
    val x         = context.mkIntConst("x")
    val y         = context.mkIntConst("y")

    context.mkGt(
      context.mkAdd(
        context.mkMod(x, 4),
        context.mkMul(3, context.mkDiv(y, 2))
      ),
      context.mkSub(x, y)
    )

  given z3Int(using context: Context): Conversion[Int, IntNum] with
    def apply(x: Int): IntNum = context.mkInt(x)

end Z3Example
