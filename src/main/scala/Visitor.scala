trait Visitor[T]:
  def visitObject(items: ObjectAccessor): Either[VisitorError, T]
  def visitArray(values: ArrayAccessor): Either[VisitorError, T]
  def visitString(string: String): Either[VisitorError, T]
  def visitNumber(number: Double): Either[VisitorError, T]
  def visitBool(bool: Boolean): Either[VisitorError, T]
  def visitNull(): Either[VisitorError, T]

sealed trait VisitorError extends ParseError:
  def message: String
  override def toString() = message
object VisitorError:
  case class Custom[E](msg: E) extends VisitorError:
    def message = msg.toString()
  case class MissMatchType(exp: String, act: String) extends VisitorError:
    // TODO get expected class from Visitor type parameter
    // TODO act: String -> enum ?
    def message = f"cannot parse $exp as $act"

given Visitor[String] with
  def visitObject(items: ObjectAccessor) = Left(
    VisitorError.MissMatchType("String", "Object")
  )
  def visitArray(values: ArrayAccessor) = Left(
    VisitorError.MissMatchType("String", "Array")
  )
  def visitString(string: String) = Right(string)
  def visitNumber(number: Double) = Left(
    VisitorError.MissMatchType("String", "Number")
  )
  def visitBool(bool: Boolean) = Left(
    VisitorError.MissMatchType("String", "Bool")
  )
  def visitNull() = Left(
    VisitorError.MissMatchType("String", "Null")
  )
