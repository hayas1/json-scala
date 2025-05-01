trait Visitor[T]:
  def visitObject(items: ObjectAccessor): T
  def visitArray(values: ArrayAccessor): T
  def visitString(string: String): T
  def visitNumber(number: Double): T
  def visitBool(bool: Boolean): T
  def visitNull(): T

class ObjectAccessor(parser: Parser):
  def nextKey[K](visitor: Visitor[K]): K =
    parser.parseString(visitor).getOrElse(throw new NotImplementedError)
  def nextValue[V](visitor: Visitor[V]): V =
    parser.parseValue(visitor).getOrElse(throw new NotImplementedError)

class ArrayAccessor(parser: Parser):
  def nextValue[V](visitor: Visitor[V]): V =
    parser.parseValue(visitor).getOrElse(throw new NotImplementedError)

given Visitor[String] with
  type Key = Unit
  type Value = Unit

  def visitObject(items: Seq[(Key, Value)]) = throw new NotImplementedError
  def visitArray(values: Seq[Value]) = throw new NotImplementedError
  def visitString(string: String) = string
  def visitNumber(number: Double) = throw new NotImplementedError
  def visitBool(bool: Boolean) = throw new NotImplementedError
  def visitNull() = throw new NotImplementedError
