package sprite.printer

import sprite.language.*

object SpritePrinter:

  def printDeclarations(declaration: List[SpriteDeclaration]): String =
    declaration.view.map(printDeclaration).mkString(";\n")

  def printDeclaration(declaration: SpriteDeclaration): String = declaration match
    case SpriteDeclaration.SuperCombinator(name, params, term) =>
      s"${(name :: params).mkString(" ")} = ${printTerm(term)}"

    case SpriteDeclaration.SuperCombinatorType(name, spriteType) =>
      s"$name : ${printType(spriteType)}"

  def printTerm(term: SpriteTerm): String = term match
    case SpriteTerm.Integer(value)      => value.toString
    case SpriteTerm.Var(name)           => name
    case SpriteTerm.Let(binding, body)  => s"let ${printBinding(binding)}; ${printTerm(body)}"
    case SpriteTerm.Lambda(param, body) => s"\\$param -> ${printTerm(body)}"
    case apply: SpriteTerm.LambdaApply  => printApply(apply)

  def printType(spriteType: SpriteType): String = spriteType match
    case SpriteType.Base(_) => printBaseType

    case SpriteType.RefinedType(base, Refinement(varName, predicate)) =>
      s"$printBaseType{$varName | ${printTerm(predicate)}}"

    case SpriteType.FunctionType(paramName, paramType: SpriteType.FunctionType, returnType) =>
      s"$paramName:(${printType(paramType)}) -> ${printType(returnType)}"

    case SpriteType.FunctionType(paramName, paramType, returnType) =>
      s"$paramName:${printType(paramType)} -> ${printType(returnType)}"

  lazy val printBaseType: String =
    "Int"

  def printBinding(binding: Bind): String =
    s"${binding.name} = ${printTerm(binding.body)}"

  def printApply(apply: SpriteTerm.LambdaApply): String =

    def printWithParensIfRec(term: SpriteTerm) =
      if term.isRecursive then s"(${printTerm(term)})" else printTerm(term)

    apply match
      case SpriteTerm.LambdaApply(apply: SpriteTerm.LambdaApply, arg) =>
        s"${printApply(apply)} ${printWithParensIfRec(arg)}"

      case SpriteTerm.LambdaApply(fun, arg) =>
        s"${printWithParensIfRec(fun)} ${printWithParensIfRec(arg)}"

end SpritePrinter
