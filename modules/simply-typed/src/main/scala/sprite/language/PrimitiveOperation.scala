package sprite.language

enum PrimitiveOperation(val name: String):
  case Not     extends PrimitiveOperation("!")
  case And     extends PrimitiveOperation("&&")
  case Or      extends PrimitiveOperation("||")
  case Add     extends PrimitiveOperation("+")
  case Sub     extends PrimitiveOperation("-")
  case Leq     extends PrimitiveOperation("<=")
  case Less    extends PrimitiveOperation("<")
  case Geq     extends PrimitiveOperation(">=")
  case Greater extends PrimitiveOperation(">")

object PrimitiveOperation:

  val byName: Map[String, PrimitiveOperation] =
    PrimitiveOperation.values.map(op => op.name -> op).toMap
