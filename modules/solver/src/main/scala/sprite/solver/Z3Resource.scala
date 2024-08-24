package sprite.solver

import cats.effect.{Resource, Sync}
import com.microsoft.z3.Context

import scala.jdk.CollectionConverters.*

final class Z3Resource[F[_]](settings: Map[String, String])(using F: Sync[F]):

  def buildContext: Resource[F, Context] =
    Resource.fromAutoCloseable(
      F.blocking(Context(settings.asJava))
    )
