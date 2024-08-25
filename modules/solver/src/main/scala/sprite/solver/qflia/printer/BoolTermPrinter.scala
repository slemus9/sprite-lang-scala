package sprite.solver.qflia.printer

import sprite.solver.qflia.language.BoolTerm

object BoolTermPrinter:

  def printTerm(term: BoolTerm): String = term match
    case BoolTerm.True                      => "true"
    case BoolTerm.False                     => "false"
    case BoolTerm.Not(p)                    => s"! ${printWithParensIfRec(p)}"
    case BoolTerm.And(p1, p2)               => s"&& ${printWithParensIfRec(p1)} ${printWithParensIfRec(p2)}"
    case BoolTerm.Or(p1, p2)                => s"|| ${printWithParensIfRec(p1)} ${printWithParensIfRec(p2)}"
    case BoolTerm.Implies(p1, p2)           => s"==> ${printWithParensIfRec(p1)} ${printWithParensIfRec(p2)}"
    case BoolTerm.Iff(p1, p2)               => s"<==> ${printWithParensIfRec(p1)} ${printWithParensIfRec(p2)}"
    case BoolTerm.Related(relation, t1, t2) =>
      s"${relation.name} ${IntTermPrinter.printWithParensIfRec(t1)} ${IntTermPrinter.printWithParensIfRec(t2)}"

  def printWithParensIfRec(term: BoolTerm) =
    if term.isRecursive then s"(${printTerm(term)})" else printTerm(term)
