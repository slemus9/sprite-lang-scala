package sprite.solver.qflia.language

enum IntPrimitive(val name: String):
  case Add extends IntPrimitive("+")
  case Sub extends IntPrimitive("-")
  case Mul extends IntPrimitive("*")

object IntPrimitive:

  type NAry = Add.type | Sub.type

  val naryByName: Map[String, NAry] =
    Map(Add.name -> Add, Sub.name -> Sub)
