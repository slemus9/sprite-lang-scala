package sprite.language

enum PrimitiveOperation(val name: String):
  case Not     extends PrimitiveOperation("!")
  case And     extends PrimitiveOperation("&")
  case Or      extends PrimitiveOperation("|")
  case Add     extends PrimitiveOperation("+")
  case Sub     extends PrimitiveOperation("-")
  case Leq     extends PrimitiveOperation("leq")
  case Less    extends PrimitiveOperation("less")
  case Geq     extends PrimitiveOperation("geq")
  case Greater extends PrimitiveOperation("greater")

object PrimitiveOperation:

  val byName: Map[String, PrimitiveOperation] =
    PrimitiveOperation.values.map(op => op.name -> op).toMap
