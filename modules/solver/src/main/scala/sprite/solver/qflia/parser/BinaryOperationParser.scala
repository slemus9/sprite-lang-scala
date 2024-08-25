package sprite.solver.qflia.parser

import cats.parse.Parser

object BinaryOperationParser:

  def binaryOperation[A, B](
      operation: Parser[Unit],
      left: Parser[A],
      right: Parser[B]
  ): Parser[(A, B)] =
    (operation *> left) ~ right
