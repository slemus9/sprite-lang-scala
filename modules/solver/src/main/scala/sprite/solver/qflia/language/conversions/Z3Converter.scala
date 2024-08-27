package sprite.solver.qflia.language.conversions

import cats.syntax.all.*
import com.microsoft.z3.*
import sprite.solver.qflia.*
import sprite.solver.qflia.language.*
import sprite.solver.qflia.language.BinaryRelation.*
import sprite.solver.qflia.language.IntTerm.*
import sprite.solver.qflia.language.VerificationCondition.Declaration
import sprite.solver.qflia.parser.IntTermParser

import scala.collection.immutable.HashMap

/** TODO: Make Z3Converter a type class, and create different instances per term type
  */
final class Z3Converter private (env: Map[String, FuncDecl[IntSort]])(using context: Context):
  import Z3Converter.*

  private def inZ3[A] = Z3Error.catchException[A]

  def toZ3Bool(term: BoolTerm): Either[VerificationConditionError, Expr[BoolSort]] = term match
    case BoolTerm.True                         => inZ3(context.mkTrue)
    case BoolTerm.False                        => inZ3(context.mkFalse)
    case BoolTerm.Not(p)                       => toZ3Bool(p).flatMap(b => inZ3(context.mkNot(b)))
    case BoolTerm.And(p1, p2)                  => binaryZ3Bool(p1, p2, context.mkAnd(_, _))
    case BoolTerm.Or(p1, p2)                   => binaryZ3Bool(p1, p2, context.mkOr(_, _))
    case BoolTerm.Implies(p1, p2)              => binaryZ3Bool(p1, p2, context.mkImplies)
    case BoolTerm.Iff(p1, p2)                  => binaryZ3Bool(p1, p2, context.mkIff)
    case BoolTerm.Related(Equal, t1, t2)       => binaryZ3Relation(t1, t2, context.mkEq)
    case BoolTerm.Related(GreaterOrEq, t1, t2) => binaryZ3Relation(t1, t2, context.mkGe)
    case BoolTerm.Related(LessOrEq, t1, t2)    => binaryZ3Relation(t1, t2, context.mkLe)
    case BoolTerm.Related(Greater, t1, t2)     => binaryZ3Relation(t1, t2, context.mkGt)
    case BoolTerm.Related(Less, t1, t2)        => binaryZ3Relation(t1, t2, context.mkLt)

  private def binaryZ3Relation(
      t1: IntTerm,
      t2: IntTerm,
      z3Op: (Expr[IntSort], Expr[IntSort]) => Expr[BoolSort]
  ): Either[VerificationConditionError, Expr[BoolSort]] =
    (toZ3Int(t1), toZ3Int(t2)).flatMapN { (i1, i2) =>
      inZ3(z3Op(i1, i2))
    }

  private def binaryZ3Bool(
      t1: BoolTerm,
      t2: BoolTerm,
      z3Op: (Expr[BoolSort], Expr[BoolSort]) => Expr[BoolSort]
  ): Either[VerificationConditionError, Expr[BoolSort]] =
    (toZ3Bool(t1), toZ3Bool(t2)).flatMapN { (b1, b2) =>
      inZ3(z3Op(b1, b2))
    }

  private def toZ3Int(term: IntTerm): Either[VerificationConditionError, Expr[IntSort]] = term match
    case IntTerm.Const(value)         => inZ3(context.mkInt(value))
    case IntTerm.Var(name)            => env.lookup(name).map(_.apply())
    case mul: IntTerm.ConstantMul     => applyZ3Mul(mul)
    case apply: IntTerm.ApplyFunction => FlattenedApply.from(apply).flatMap(applyZ3Function)

  private def applyZ3Mul(mul: ConstantMul): Either[VerificationConditionError, Expr[IntSort]] =
    toZ3Int(mul.term).flatMap { z3Term =>
      inZ3(context.mkMul(context.mkInt(mul.constant), z3Term))
    }

  private def applyZ3Function(
      apply: FlattenedApply
  ): Either[VerificationConditionError, Expr[IntSort]] =
    apply.args.traverse(toZ3Int).flatMap { z3Args =>
      apply.functionName match
        case IntPrimitive.Add.name => inZ3(context.mkAdd(z3Args.toList*))
        case IntPrimitive.Sub.name => inZ3(context.mkSub(z3Args.toList*))
        case uninterpreted         =>
          env.lookup(uninterpreted).flatMap { function =>
            inZ3(function.apply(z3Args.toList*))
          }
    }

end Z3Converter

object Z3Converter:

  type DeclarationEnv = Map[String, FuncDecl[IntSort]]

  def from(declarations: List[Declaration])(using Context): Either[Z3Error, Z3Converter] =
    declarationEnvironment(declarations).map(Z3Converter(_))

  private def declarationEnvironment(
      declarations: List[Declaration]
  )(using context: Context): Either[Z3Error, DeclarationEnv] =
    declarations
      .traverse {
        case decl: Declaration.Const    => constantDeclaration(decl).map(decl.name -> _)
        case decl: Declaration.Function => functionDeclaration(decl).map(decl.name -> _)
      }
      .map(_.toMap)

  private def constantDeclaration(
      declaration: Declaration.Const
  )(using context: Context): Either[Z3Error, FuncDecl[IntSort]] =
    Z3Error.catchException(
      context.mkConstDecl(declaration.name, context.getIntSort)
    )

  private def functionDeclaration(
      declaration: Declaration.Function
  )(using context: Context): Either[Z3Error, FuncDecl[IntSort]] =
    Z3Error.catchException {
      val domain = Array.fill[Sort](declaration.arity)(context.getIntSort)
      context.mkFuncDecl(declaration.name, domain, context.getIntSort)
    }

  extension (env: DeclarationEnv)
    def lookup(key: String): Either[DeclarationNotFound, FuncDecl[IntSort]] =
      env.get(key).toRight(DeclarationNotFound(key))
