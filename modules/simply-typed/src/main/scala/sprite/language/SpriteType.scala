package sprite.language

import sprite.solver.qflia.language.*

enum SpriteType:
  case Base(base: SpriteBaseType)
  case RefinedType(base: SpriteBaseType, refinement: Refinement)
  case FunctionType(param: String, paramType: SpriteType, returnType: SpriteType)

  def substitute(variable: String, term: IntTerm): SpriteType = this match
    case b: Base => b

    case r @ RefinedType(_, Refinement(v, p)) if v == variable => r

    case RefinedType(b, Refinement(v, p)) =>
      RefinedType(b, Refinement(v, p.substitute(variable, term)))

    case FunctionType(x, s, t) if x == variable =>
      FunctionType(x, s.substitute(variable, term), t)

    case FunctionType(x, s, t) =>
      FunctionType(x, s.substitute(variable, term), t.substitute(variable, term))

enum SpriteBaseType:
  case Int

final case class Refinement(param: String, predicate: BoolTerm)

object SpriteType:

  /** Builds the type: Int{v | v == constant}
    */
  def primitive(constant: SpriteTerm.Integer): RefinedType =
    RefinedType(
      base = SpriteBaseType.Int,
      refinement = selfRefinedEquality(IntTerm.Const(constant.value))
    )

  /** Builds the type: x:Int -> y:Int -> Int{v | v == op x y} where op = + | -
    */
  def primitive(operation: PrimitiveOperation.IntOperation): FunctionType =
    val param1   = "x"
    val param2   = "y"
    val addition = IntTerm.ApplyFunction(
      fun = IntTerm.ApplyFunction(IntTerm.Var(operation.name), IntTerm.Var(param1)),
      arg = IntTerm.Var(param2)
    )

    FunctionType(
      param = param1,
      paramType = Base(SpriteBaseType.Int),
      returnType = FunctionType(
        param = param2,
        paramType = Base(SpriteBaseType.Int),
        returnType = RefinedType(
          base = SpriteBaseType.Int,
          refinement = selfRefinedEquality(addition)
        )
      )
    )

  /** Builds the refinement: {v | v == term}
    */
  def selfRefinedEquality(term: IntTerm): Refinement =
    val param = "v"
    Refinement(
      param,
      predicate = BoolTerm.Related(
        relation = BinaryRelation.Equal,
        t1 = IntTerm.Var(param),
        t2 = term
      )
    )

end SpriteType
