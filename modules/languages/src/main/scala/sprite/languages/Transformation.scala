package sprite.languages

trait Transformation[F[_], From, To]:

  def transform(from: From): F[To]

object Transformation:

  extension [F[_], A](from: A)
    def to[B](using trans: Transformation[F, A, B]): F[B] =
      trans.transform(from)
