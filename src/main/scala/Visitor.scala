trait Visitor[T]:
  def expectType: List[ValueType]
  def visitObject(accessor: ObjectAccessor): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Object))
  def visitArray(accessor: ArrayAccessor): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Array))
  def visitString(string: String): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.String))
  def visitNumber(number: Double): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Number))
  def visitBool(bool: Boolean): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Bool))
  def visitNull(): Either[VisitorError, T] =
    Left(VisitorError.MissMatchType(expectType, ValueType.Null))

sealed trait VisitorError extends ParseError
object VisitorError:
  case class Custom[E](msg: E) extends VisitorError:
    def message = msg.toString()
  case class Parsing[E <: ParseError](source: E) extends VisitorError:
    def message = source.message
  case class MissMatchType(exp: List[ValueType], act: ValueType)
      extends VisitorError:
    def message =
      val or = exp.map(_.toString).mkString(" or ")
      f"cannot parse $act as $or"
  case class MissingField(typename: String, field: String) extends VisitorError:
    def message = f"missing field $typename.$field"
  case class UnexpectedField(typename: String, field: String)
      extends VisitorError:
    def message = f"$typename has no field such as $field"

given Visitor[String] with
  def expectType = List(ValueType.String)
  override def visitString(string: String) = Right(string)
