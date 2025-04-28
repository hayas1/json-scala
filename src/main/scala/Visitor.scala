trait Visitor[T]:
  def visit_object(fields: Seq[(String, Json)]): T // TODO stream ?
  def visit_array(values: Seq[Json]): T // TODO stream ?
  def visit_string(value: String): T
  def visit_number(value: Double): T
  def visit_bool(value: Boolean): T
  def visit_null(): T
