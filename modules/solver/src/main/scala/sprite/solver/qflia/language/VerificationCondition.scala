package sprite.solver.qflia.language

final case class VerificationCondition(
    declarations: List[VerificationCondition.Declaration],
    assertions: List[BoolTerm]
)

object VerificationCondition:

  /** For now, all declarations have the Integer Sort. This will be generalized later
    */
  enum Declaration:
    case Const(name: String)
    case Function(name: String, arity: Int)
