import org.scalatest.funsuite.AnyFunSuite

class ValueTest extends AnyFunSuite:
  test("null is null") {
    val json = Json.ValueNull

    assert(json.isNull())
  }

  test("object is not null") {
    val json = Json.ValueObject(Map.empty)

    assert(!json.isNull())
  }
