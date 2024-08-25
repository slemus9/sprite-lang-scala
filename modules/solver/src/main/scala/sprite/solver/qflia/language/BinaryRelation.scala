package sprite.solver.qflia.language

enum BinaryRelation(val name: String):
  case Equal       extends BinaryRelation("==")
  case GreaterOrEq extends BinaryRelation(">=")
  case LessOrEq    extends BinaryRelation("<=")
  case Greater     extends BinaryRelation(">")
  case Less        extends BinaryRelation("<")

object BinaryRelation:

  def byName: Map[String, BinaryRelation] =
    BinaryRelation.values.map(rel => rel.name -> rel).toMap
