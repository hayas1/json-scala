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

  case class Person(name: String)
  given Visitor[Person] with // TODO derive
    def expectType = List(ValueType.Object)
    override def visitObject(accessor: ObjectAccessor) =
      val typename = "Person"
      var (name): (Either[VisitorError, String]) = (
        Left(VisitorError.MissingField(typename, "name"))
      )
      accessor.nextName[String]() match
        case Right("name") =>
          name = accessor.nextValue[String]().left.map(VisitorError.Parsing(_))
        case Right(field) => Left(VisitorError.UnexpectedField(typename, field))
        case Left(e)      => Left(e)
      for {
        name <- name
      } yield Person(name)
  test("object as defined class") {
    val input = """{"name": "Taro"}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json == Right(Person("Taro"))
    )
  }
  test("object as defined class with missing field") {
    val input = """{"age": 20}""".stripMargin
    val json = parseJson[Person](input)

    assert(
      json.left.get.toString == "(1, 1) to (1, 8): missing field Person.name"
    )
  }
