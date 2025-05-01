trait Visitor[T]:
  def visitObject(items: ObjectAccessor): T
  def visitArray(values: ArrayAccessor): T
  def visitString(string: String): T
  def visitNumber(number: Double): T
  def visitBool(bool: Boolean): T
  def visitNull(): T

given Visitor[String] with
  def visitObject(items: ObjectAccessor) = throw new NotImplementedError
  def visitArray(values: ArrayAccessor) = throw new NotImplementedError
  def visitString(string: String) = string
  def visitNumber(number: Double) = throw new NotImplementedError
  def visitBool(bool: Boolean) = throw new NotImplementedError
  def visitNull() = throw new NotImplementedError
