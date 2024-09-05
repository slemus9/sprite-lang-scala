package sprite.solver.qflia.transformations

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.microsoft.z3.{BoolSort, Expr}
import sprite.languages.Transformation.transform
import sprite.solver.qflia.language.Constraint
import sprite.solver.qflia.language.Constraint.*
import sprite.solver.qflia.transformations.given
import sprite.solver.qflia.transformations.Z3Transformation.Result

object ConstraintZ3Interpreter:

  def interpret(
      constraint: Constraint
  )(using context: Z3Context): Result[NonEmptyList[Expr[BoolSort]]] = constraint match
    case Predicate(p) =>
      p.transform[Result].into.map(NonEmptyList.one)

    case And(c1, c2) =>
      (interpret(c1), interpret(c2)).mapN(_ concatNel _)

    case ForallImplication(param, antecedent, consequent) =>
      val intSort = context.z3.getIntSort
      for
        paramDecl     <- context.call(_.mkConstDecl(param, intSort))
        z3Antecendent <- antecedent.transform[Result].into[Expr[BoolSort]]
        z3Consequent  <- interpret(consequent)(using context.addDeclaration(param, paramDecl))
      yield z3Antecendent :: z3Consequent
