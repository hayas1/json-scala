import cats.{Functor, Traverse, Applicative, Eval}

class RowColIterator(mat: Seq[Seq[Char]], start: Position = Position())
    extends Iterator[Char]:
  private var cursor = start

  def hasNext = !cursor.isEndOfMat(mat)
  def next() =
    val curr = peek
    cursor = if cursor.isEndOfRow(mat) then cursor.nextRow else cursor.nextCol
    curr

  def peek = if cursor.isEndOfRow(mat) then '\n' else cursor.index(mat)
  def position = cursor

case class Position(row: Int = 0, col: Int = 0):
  def nextRow = Position(row + 1, 0)
  def nextCol = Position(row, col + 1)
  def isEndOfRow[T](mat: Seq[Seq[T]]) =
    isEndOfMat(mat) || col >= mat(row).length
  def isEndOfMat[T](mat: Seq[Seq[T]]) = row >= mat.length
  def index[T](mat: Seq[Seq[T]]) = mat(row)(col)
  def oneIndexed = Position(row + 1, col + 1)
  def asSpan = Span(this, this)
given Ordering[Position] =
  Ordering.Tuple2(Ordering.Int, Ordering.Int).on(p => (p.row, p.col))

case class Span(start: Position, end: Position):
  override def toString() =
    val (start1, end1) = (start.oneIndexed, end.oneIndexed)
    if start1 == end1 then s"row ${start1.row}, col ${start1.col}"
    else s"(${start1.row}, ${start1.col}) to (${end1.row}, ${end1.col})"
  def merged(other: Span) = Span(
    summon[Ordering[Position]].min(start, other.start),
    summon[Ordering[Position]].max(end, other.end)
  )

case class Spanned[T](target: T, span: Span)
given Traverse[Spanned] with
  def traverse[G[_], A, B](fa: Spanned[A])(
      f: A => G[B]
  )(using G: Applicative[G]): G[Spanned[B]] =
    G.map(f(fa.target))(b => Spanned(b, fa.span))
  def foldLeft[A, B](fa: Spanned[A], b: B)(f: (B, A) => B): B =
    f(b, fa.target)
  def foldRight[A, B](fa: Spanned[A], lb: Eval[B])(
      f: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    f(fa.target, lb)
