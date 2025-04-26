import java.{util => ju}

class RowColIterator(mat: List[List[Char]], start: Position = Position())
    extends Iterator[Char]:
  private var cursor = start

  def hasNext = !cursor.isEndOfMat(mat)
  def next() =
    val curr = peek
    if cursor.isEndOfRow(mat) then cursor = cursor.nextRow
    else cursor = cursor.nextCol
    curr

  def peek = if cursor.isEndOfRow(mat) then '\n' else cursor.index(mat)
  def position = cursor

case class Position(row: Int = 0, col: Int = 0):
  def nextRow = Position(row + 1, 0)
  def nextCol = Position(row, col + 1)
  def isEndOfRow[T](mat: List[List[T]]) = col >= mat(row).length
  def isEndOfMat[T](mat: List[List[T]]) = row >= mat.length
  def index[T](mat: List[List[T]]) = mat(row)(col)

case class Span(val start: Position, val end: Position)

case class Token(val raw: RawToken, val span: Span)

enum RawToken:
  case LeftBrace
  case RightBrace
  case Quote
  case Raw(raw: String)
