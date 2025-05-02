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
      json.left.get.toString == "cannot parse Object as String"
    )
  }
