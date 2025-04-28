trait Visitor[T]:
  def visit_object(items: Seq[(T, T)]): T // TODO stream ?
  def visit_array(values: Seq[T]): T // TODO stream ?
  def visit_string(string: String): T
  def visit_number(number: Double): T
  def visit_bool(bool: Boolean): T
  def visit_null(): T
