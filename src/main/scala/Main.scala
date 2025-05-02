@main def hello(): Unit =
  val input = """{"hello": "world"}"""
  val json = parseJson[Json](input)
  println(json)
