enum Json:
  case ValueObject(members: Map[String, Json])
  case ValueArray(elements: Seq[Json])
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
  def visitObject(members: ObjectAccessor) = Right(
    Json.ValueObject(members.toIter[String, Json].toMap)
  )
  def visitArray(elements: ArrayAccessor) = Right(
    Json.ValueArray(elements.toIter.toSeq)
  )
  def visitString(string: String) = Right(Json.ValueString(string))
  def visitNumber(number: Double) = Right(Json.ValueNumber(number))
  def visitBool(bool: Boolean) = Right(Json.ValueBool(bool))
  def visitNull() = Right(Json.ValueNull)
