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
  def parse(input: String)(using
      visitor: Visitor[Json],
      kv: Visitor[visitor.Key],
      vv: Visitor[visitor.Value]
  ) = Parser(input).parseValue[Json]

given Visitor[Json] with
  type Key = String
  type Value = Json
  def visit_object(items: Seq[(Key, Value)]) =
    Json.ValueObject(items.map((k, v) => k -> v).toMap)
  def visit_array(values: Seq[Value]) = Json.ValueArray(values.toSeq)
  def visit_string(string: String) = Json.ValueString(string)
  def visit_number(number: Double) = Json.ValueNumber(number)
  def visit_bool(bool: Boolean) = Json.ValueBool(bool)
  def visit_null() = Json.ValueNull
