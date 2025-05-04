import org.scalatest.funsuite.AnyFunSuite

class ParseTest extends AnyFunSuite:
  test("null is null") {
    val input = """null"""
    val json = parseJson[Json](input)

    assert(json == Right(Json.ValueNull))
  }

  test("object consisted of string") {
    val input = """{"hello": "world"}""".stripMargin
    val json = parseJson[Json](input)

    assert(
      json == Right(Json.ValueObject(Map("hello" -> Json.ValueString("world"))))
    )
  }

  test("object consisted of string and null") {
    val input = """{"hello": "world", "null": null}""".stripMargin
    val json = parseJson[Json](input)

    assert(
      json == Right(
        Json.ValueObject(
          Map(
            "hello" -> Json.ValueString("world"),
            "null" -> Json.ValueNull
          )
        )
      )
    )
  }

  test("array consisted of int") {
    val input = """[1, 2, 3]""".stripMargin
    val json = parseJson[Json](input)

    assert(
      json == Right(
        Json.ValueArray(
          List(Json.ValueNumber(1), Json.ValueNumber(2), Json.ValueNumber(3))
        )
      )
    )
  }

  test("invalid object structure") {
    val input = """{"hello": "world" "null": null}""".stripMargin
    val json = parseJson[Json](input)

    assert(
      json.left.get.toString == "row 1, col 19: expected '}', but got '\"'"
    )
  }

  test("array of object") {
    val input = """[{"hello": "world"}, {"null": null}]""".stripMargin
    val json = parseJson[Json](input)

    assert(
      json == Right(
        Json.ValueArray(
          List(
            Json.ValueObject(Map("hello" -> Json.ValueString("world"))),
            Json.ValueObject(Map("null" -> Json.ValueNull))
          )
        )
      )
    )
  }
