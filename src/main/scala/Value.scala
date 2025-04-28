enum Json:
  case ValueObject(fields: Map[String, Json])
  case ValueArray(values: Seq[Json])
  case ValueString(value: String)
  case ValueNumber(value: Double)
  case ValueBool(value: Boolean)
  case ValueNull

  def isNull(): Boolean = this match
    case ValueNull => true
    case _         => false
  def asString: Option[String] = this match
    case ValueString(value) => Some(value)
    case _                  => None

object Json:
  def parse(input: String)(using visitor: Visitor[Json]) =
    Parser(input).parseValue(visitor)

given Visitor[Json] with
  def visit_object(items: Seq[(Json, Json)]) =
    Json.ValueObject(
      items
        .map((k, v) => k.asString.get -> v)
        .toMap
    )
  def visit_array(values: Seq[Json]) = Json.ValueArray(values)
  def visit_string(string: String) = Json.ValueString(string)
  def visit_number(number: Double) = Json.ValueNumber(number)
  def visit_bool(bool: Boolean) = Json.ValueBool(bool)
  def visit_null() = Json.ValueNull
