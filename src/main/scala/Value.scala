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
  def visitObject(items: Seq[(Key, Value)]) =
    Json.ValueObject(items.map((k, v) => k -> v).toMap)
  def visitArray(values: Seq[Value]) = Json.ValueArray(values.toSeq)
  def visitString(string: String) = Json.ValueString(string)
  def visitNumber(number: Double) = Json.ValueNumber(number)
  def visitBool(bool: Boolean) = Json.ValueBool(bool)
  def visitNull() = Json.ValueNull
