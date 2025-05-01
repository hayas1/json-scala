enum Json:
  case ValueObject(items: Map[String, Json])
  case ValueArray(values: Seq[Json])
  case ValueString(string: String)
  case ValueNumber(number: Double)
  case ValueBool(bool: Boolean)
  case ValueNull

  def isNull(): Boolean = this match
    case ValueNull => true
    case _         => false
  def asString: Option[String] = this match
    case ValueString(string) => Some(string)
    case _                   => None

object Json:
  def parse(input: String)(using Visitor[Json]) = Parser(input).parseValue()

given Visitor[Json] with
  def visitObject(items: ObjectAccessor) =
    Json.ValueObject(items.toIter[String, Json].toMap)
  def visitArray(values: ArrayAccessor) = Json.ValueArray(values.toIter.toSeq)
  def visitString(string: String) = Json.ValueString(string)
  def visitNumber(number: Double) = Json.ValueNumber(number)
  def visitBool(bool: Boolean) = Json.ValueBool(bool)
  def visitNull() = Json.ValueNull
