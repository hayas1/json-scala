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
  def parse(input: String)(
      visitor: Visitor[Json]
  ) = Parser(input).parseValue(visitor)

given Visitor[Json] with
  def visitObject(items: ObjectAccessor) =
    Json.ValueObject(items.map((k, v) => k -> v).toMap)
  def visitArray(values: ArrayAccessor) = Json.ValueArray(values.toSeq)
  def visitString(string: String) = Json.ValueString(string)
  def visitNumber(number: Double) = Json.ValueNumber(number)
  def visitBool(bool: Boolean) = Json.ValueBool(bool)
  def visitNull() = Json.ValueNull
