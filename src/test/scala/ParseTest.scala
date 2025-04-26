import org.scalatest.funsuite.AnyFunSuite

class ParseTest extends AnyFunSuite:
  test("null is null") {
    val input = """null"""
    val json = Json.parse(input)

    assert(json == Json.ValueNull)
  }

  test("object consisted of string") {
    val input = """{"hello": "world"}""".stripMargin
    val json = Json.parse(input)

    assert(json == Json.ValueObject(Map("hello" -> Json.ValueString("world"))))
  }
