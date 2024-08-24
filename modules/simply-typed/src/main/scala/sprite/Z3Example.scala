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

      val predicate = examplePredicate(context)
      val result    = solver.check(predicate)

      IO.println(s"Result: $result") >> IO.println(solver.getModel())

  def z3ContextResource: Resource[IO, Context] =
    Resource.fromAutoCloseable(IO.blocking(Context()))

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
