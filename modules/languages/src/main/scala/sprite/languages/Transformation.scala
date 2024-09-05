package sprite.languages

import cats.syntax.traverse.*
import cats.Applicative

trait Transformation[F[+_], -From, +To]:
  export Transformation.*

  def convert(from: From): F[To]

object Transformation:

  extension [A](from: A)
    def transform[F[+_]]: TransformSingle[F, A] =
      TransformSingle(from)

  extension [A](from: List[A])
    def transformMany[F[+_]: Applicative]: TransformMany[F, A] =
      TransformMany(from)

  final class TransformSingle[F[+_], A](from: A):

    def into[B](using trans: Transformation[F, A, B]): F[B] =
      trans.convert(from)

  final class TransformMany[F[+_]: Applicative, A](from: List[A]):

    def into[B](using trans: Transformation[F, A, B]): F[List[B]] =
      from.traverse(trans.convert)
