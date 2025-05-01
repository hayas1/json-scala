trait Visitor[T]:
  def visitObject(items: ObjectAccessor): Either[Visitor.VisitorError, T]
  def visitArray(values: ArrayAccessor): Either[Visitor.VisitorError, T]
  def visitString(string: String): Either[Visitor.VisitorError, T]
  def visitNumber(number: Double): Either[Visitor.VisitorError, T]
  def visitBool(bool: Boolean): Either[Visitor.VisitorError, T]
  def visitNull(): Either[Visitor.VisitorError, T]

object Visitor:
  sealed trait VisitorError extends Parser.ParserError:
    def message: String
    override def toString() = message
  case class Custom[E](msg: E) extends VisitorError:
    def message = msg.toString()
  case class MissMatchType(exp: String, act: String) extends VisitorError:
    // TODO get expected class from Visitor type parameter
    // TODO act: String -> enum ?
    def message = f"cannot parse $exp as $act"

given Visitor[String] with
  def visitObject(items: ObjectAccessor) = Left(
    Visitor.MissMatchType("String", "Object")
  )
  def visitArray(values: ArrayAccessor) = Left(
    Visitor.MissMatchType("String", "Array")
  )
  def visitString(string: String) = Right(string)
  def visitNumber(number: Double) = Left(
    Visitor.MissMatchType("String", "Number")
  )
  def visitBool(bool: Boolean) = Left(
    Visitor.MissMatchType("String", "Bool")
  )
  def visitNull() = Left(
    Visitor.MissMatchType("String", "Null")
  )
