package sprite.solver.qflia.language

enum Constraint:

  case Predicate(value: BoolTerm)

  case And(c1: Constraint, c2: Constraint)

  /** Encodes the constraint: forall param: Int . antecedent ==> consequent. For now the sort of
    * param is Int, this will be generalized later
    */
  case ForallImplication(param: String, antecedent: BoolTerm, consequent: Constraint)

  def and(that: Constraint): And =
    And(this, that)

object Constraint:

  val alwaysHolds: Predicate =
    Predicate(BoolTerm.True)

  def of(term: BoolTerm): Predicate =
    Predicate(term)

  def forall(param: String, antecedent: BoolTerm, consequent: Constraint): ForallImplication =
    ForallImplication(param, antecedent, consequent)
