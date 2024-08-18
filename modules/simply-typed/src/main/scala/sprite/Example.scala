package sprite

import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object Example extends IOApp.Simple:

  val program1 = """
    f : x:Int -> y:(h:Int -> Bool) -> z:Int{v | less v (+ x 10)} -> Bool;
    f x y z = less z (+ x 20);

    x : h:(a:Int -> Bool) -> Int;
    x =
      let h = 2;
      let y = - 10 5;
      + h y;
  """

  val program2 = "f : h:(a:Int -> Bool) -> g:(b:Bool -> Int) -> Bool"

  override def run: IO[Unit] =
    IO.println(
      parser.SpriteParser.spriteDeclarations.parseAll(program1).right.get.toList
    )
