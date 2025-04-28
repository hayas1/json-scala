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
  def visit_object(fields: Seq[(String, Json)]) = Json.ValueObject(fields.toMap)
  def visit_array(values: Seq[Json]) = Json.ValueArray(values)
  def visit_string(value: String) = Json.ValueString(value)
  def visit_number(value: Double) = Json.ValueNumber(value)
  def visit_bool(value: Boolean) = Json.ValueBool(value)
  def visit_null() = Json.ValueNull
