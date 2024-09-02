package sprite.solver.qflia.substitutions

import sprite.languages.Substitution
import sprite.languages.Substitution.substitute
import sprite.solver.qflia.language.BoolTerm
import sprite.solver.qflia.language.BoolTerm.*
import sprite.solver.qflia.language.IntTerm

given BoolTermSubstitution: Substitution[BoolTerm, IntTerm] with

  override def substitute(term: BoolTerm, variable: String, body: IntTerm): BoolTerm = term match
    case True                      => True
    case False                     => False
    case Not(p)                    => Not(substitute(p, variable, body))
    case And(p1, p2)               => And(substitute(p1, variable, body), substitute(p2, variable, body))
    case Or(p1, p2)                => Or(substitute(p1, variable, body), substitute(p2, variable, body))
    case Implies(p1, p2)           => Implies(substitute(p1, variable, body), substitute(p2, variable, body))
    case Iff(p1, p2)               => Iff(substitute(p1, variable, body), substitute(p2, variable, body))
    case Related(relation, t1, t2) =>
      Related(
        relation,
        t1.substitute(variable, body),
        t2.substitute(variable, body)
      )
