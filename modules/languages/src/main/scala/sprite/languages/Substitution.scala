package sprite.languages

trait Substitution[A, B]:

  def substitute(term: A, variable: String, body: B): A

object Substitution:

  extension [A](term: A)
    def substitute[B](variable: String, body: B)(using subs: Substitution[A, B]): A =
      subs.substitute(term, variable, body)
