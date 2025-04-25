@main def hello(): Unit =
  val input = """{"hello": "world"}"""
  val json = Json.parse(input)
  println(json)

