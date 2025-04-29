trait Visitor[T]:
  type Key
  type Value

  def visitObject(items: Seq[(Key, Value)]): T
  def visitArray(values: Seq[Value]): T
  def visitString(string: String): T
  def visitNumber(number: Double): T
  def visitBool(bool: Boolean): T
  def visitNull(): T

given Visitor[String] with
  type Key = Unit
  type Value = Unit

  def visitObject(items: Seq[(Key, Value)]) = throw new NotImplementedError
  def visitArray(values: Seq[Value]) = throw new NotImplementedError
  def visitString(string: String) = string
  def visitNumber(number: Double) = throw new NotImplementedError
  def visitBool(bool: Boolean) = throw new NotImplementedError
  def visitNull() = throw new NotImplementedError
