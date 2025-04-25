import org.scalatest.funsuite.AnyFunSuite

class ParseTest extends AnyFunSuite:
  test("null is null") {
    val input = """null"""
    val json = Json.parse(input)

    assert(json == Json.ValueNull)
  }
