import cats.syntax.traverse.*
import cats.instances.list.*

enum Json:
  case ValueObject(members: Map[String, Json])
  case ValueArray(elements: Seq[Json])
  case ValueString(string: String)
  case ValueNumber(number: Double)
  case ValueBool(bool: Boolean)
  case ValueNull

  def valueType = this match
    case ValueObject(_) => ValueType.Object
    case ValueArray(_)  => ValueType.Array
    case ValueString(_) => ValueType.String
    case ValueNumber(_) => ValueType.Number
    case ValueBool(_)   => ValueType.Bool
    case ValueNull      => ValueType.Null

  def isNull(): Boolean = this match
    case ValueNull => true
    case _         => false
  def asString: Option[String] = this match
    case ValueString(string) => Some(string)
    case _                   => None

enum ValueType:
  case Object
  case Array
  case String
  case Number
  case Bool
  case Null

given Visitor[Json] with
  def expectType = List(
    ValueType.Object,
    ValueType.Array,
    ValueType.String,
    ValueType.Number,
    ValueType.Bool,
    ValueType.Null
  )
  override def visitObject(accessor: ObjectAccessor) =
    (for {
      pairs <- accessor.pairs[String, Json].toList.sequence
    } yield Json.ValueObject(pairs.toMap)).left.map(VisitorError.Parsing(_))
  override def visitArray(accessor: ArrayAccessor) =
    (for {
      elements <- accessor.elements.toList.sequence
    } yield Json.ValueArray(elements)).left.map(VisitorError.Parsing(_))
  override def visitString(string: String) = Right(Json.ValueString(string))
  override def visitNumber(number: Double) = Right(Json.ValueNumber(number))
  override def visitBool(bool: Boolean) = Right(Json.ValueBool(bool))
  override def visitNull() = Right(Json.ValueNull)
