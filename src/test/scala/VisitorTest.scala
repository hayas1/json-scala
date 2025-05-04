import org.scalatest.funsuite.AnyFunSuite

class VisitorTest extends AnyFunSuite:
  test("string as string") {
    val input = """ "scala3" """.stripMargin
    val json = parseJson[String](input)

    assert(
      json == Right("scala3")
    )
  }

  test("object as string") {
    val input = """{}""".stripMargin
    val json = parseJson[String](input)

    assert(
      json.left.get.toString == "(1, 1) to (1, 2): cannot parse Object as String"
    )
  }

  case class Person(name: String, age: Int) derives Visitor
  test("object as defined case class") {
    val input = """{"name": "Taro", "age": 20}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json == Right(Person("Taro", 20))
    )
  }
  test("object as defined case class (random order)") {
    val input = """{"age": 20, "name": "Taro"}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json == Right(Person("Taro", 20))
    )
  }
  test("object as defined case class with missing field") {
    val input = """{"age": 20}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json.left.get.toString == "(1, 1) to (1, 11): missing field Person.name"
    )
  }
  test("object as defined case class with unexpected field") {
    val input = """{"name": "Taro", "address": "Tokyo"}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json.left.get.toString == "(1, 1) to (1, 28): Person has no field such as address"
    )
  }
