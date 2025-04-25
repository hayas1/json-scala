enum Json:
  case ValueNull
  case ValueBool(value: Boolean)
  case ValueNumber(value: Double)
  case ValueString(value: String)
  case ValueArray(values: List[Json])
  case ValueObject(fields: Map[String, Json])

  def isNull(): Boolean = this match
    case ValueNull => true
    case _         => false

object Json:
  def parse(input: String): Json =
    Parser.parse(input)
