trait Visitor[T]:
  type Key
  type Value

  def visit_object(items: Seq[(Key, Value)]): T
  def visit_array(values: Seq[Value]): T
  def visit_string(string: String): T
  def visit_number(number: Double): T
  def visit_bool(bool: Boolean): T
  def visit_null(): T

given Visitor[String] with
  type Key = Unit
  type Value = Unit

  def visit_object(items: Seq[(Key, Value)]) =
    throw new NotImplementedError
  def visit_array(values: Seq[Value]) = throw new NotImplementedError
  def visit_string(string: String) = string
  def visit_number(number: Double) = throw new NotImplementedError
  def visit_bool(bool: Boolean) = throw new NotImplementedError
  def visit_null() = throw new NotImplementedError
