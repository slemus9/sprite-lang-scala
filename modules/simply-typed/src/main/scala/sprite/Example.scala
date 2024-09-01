package sprite

import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object Example extends IOApp.Simple:

  val program1 = """
    f : x:Int -> h:(y:Int -> Int) -> z:Int{v | <==> (> v (+ x 10)) (== (h x) (+ z 1))} -> Int;
    f = \ x h z -> > z (+ x 20);

    x : h:(a:Int -> Int) -> Int;
    x =
      let someFun = \ x y z -> + (+ (x y)) z;
      let h = 2;
      let y = - 10 5;
      + h y;
  """

  override def run: IO[Unit] =
    IO.println(
      parser.SpriteParser.spriteDeclarations.parseAll(program1)
    )
