enum Json:
  case ValueObject(fields: Map[String, Json])
  case ValueArray(values: List[Json])
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
  def parse(input: String) =
    Parser(input).parse_value()
