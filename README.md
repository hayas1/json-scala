## Json Scala
A JSON parser written in Scala.

### Usage
Parse JSON with known schema
```scala
  case class Person(name: String, age: Int) derives Visitor

  val input = """{"name": "Taro", "age": 20}""".stripMargin
  val json = parseJson[Person](input)

  assert(
    json == Right(Person("Taro", 20))
  )
```

Parse JSON with unknown schema
```scala
  val input = """[null, true, 1]""".stripMargin
  val json = parseJson[Json](input)

  assert(
    json == Right(
      Json.ValueArray(
        List(Json.ValueNull, Json.ValueBool(true), Json.ValueNumber(1))
      )
    )
  )
```
