package sprite.printer

import sprite.language.*
import weaver.FunSuite

object SpritePrinterTests extends FunSuite:

  test("Int constant should be printed as is"):
    expect.same(
      expected = "1234451",
      found = SpritePrinter.printTerm(SpriteTerm.IntConst(1234451))
    ) and expect.same(
      expected = "0",
      found = SpritePrinter.printTerm(SpriteTerm.IntConst(0))
    ) and expect.same(
      expected = "-534331",
      found = SpritePrinter.printTerm(SpriteTerm.IntConst(-534331))
    )

  test("Variable name should be printed as is"):
    expect.same(
      expected = "myvariable",
      found = SpritePrinter.printTerm(SpriteTerm.Var("myvariable"))
    )

  test("Binding should be printed as a string with the binding name and the term separated by '='"):
    val binding = Bind(name = "mybinding", body = SpriteTerm.IntConst(-534331))

    expect.same(
      expected = "mybinding = -534331",
      found = SpritePrinter.printBinding(binding)
    )

  test(
    "Let binding should be printed as a string starting with 'let', " +
      "followed by the binding and the body separated by ';'"
  ):
    val letBinding = SpriteTerm.Let(
      binding = Bind(name = "x", body = SpriteTerm.IntConst(-534331)),
      body = SpriteTerm.LambdaApply(
        fun = SpriteTerm.Var("f"),
        arg = SpriteTerm.Var("x")
      )
    )

    expect.same(
      expected = "let x = -534331; f x",
      found = SpritePrinter.printTerm(letBinding)
    )

end SpritePrinterTests
