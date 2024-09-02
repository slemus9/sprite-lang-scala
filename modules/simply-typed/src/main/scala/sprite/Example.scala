package sprite

import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object Example extends IOApp.Simple:

  val program1 = """
    f : x:Int -> h:(y:Int -> Int) -> z:Int{v | <==> (> v (+ x 10)) (== (h x) (+ z 1))} -> Int;
    f = \ x h z ->
      let y = + x 20;
      > z y;

    x : h:(a:Int -> Int) -> Int;
    x =
      let someFun = \ x y z -> let w = - y z; let a = 10; + x y z a w;
      let h = 2;
      let y = - 10 5;
      + h y;
  """

  override def run: IO[Unit] =
    IO.println(
      parser.SpriteParser.spriteDeclarations.parseAll(program1)
    )
