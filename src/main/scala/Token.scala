import java.util.ResourceBundle.Control
abstract class ControlToken(represent: Char):
  override def toString = represent.toString()
case class LeftBrace() extends ControlToken(ControlToken.LEFT_BRACE)
case class RightBrace() extends ControlToken(ControlToken.RIGHT_BRACE)
case class Colon() extends ControlToken(ControlToken.COLON)
case class Quote() extends ControlToken(ControlToken.QUOTE)

object ControlToken:
  val LEFT_BRACE = '{'
  val RIGHT_BRACE = '}'
  val COLON = ':'
  val QUOTE = '"'

  def apply(represent: Char) = represent match
    case LEFT_BRACE  => Some(LeftBrace)
    case RIGHT_BRACE => Some(RightBrace)
    case COLON       => Some(Colon)
    case QUOTE       => Some(Quote)
    case _           => None

class RowColIterator(mat: List[List[Char]], start: Position = Position())
    extends Iterator[Char]:
  private var cursor = start

  def hasNext = !cursor.isEndOfMat(mat)
  def next() =
    val curr = peek
    cursor = if cursor.isEndOfRow(mat) then cursor.nextRow else cursor.nextCol
    curr

  def peek = if cursor.isEndOfRow(mat) then '\n' else cursor.index(mat)
  def position = cursor
  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while peek.isWhitespace do next()
    this

case class Position(row: Int = 0, col: Int = 0):
  def nextRow = Position(row + 1, 0)
  def nextCol = Position(row, col + 1)
  def isEndOfRow[T](mat: List[List[T]]) =
    isEndOfMat(mat) || col >= mat(row).length
  def isEndOfMat[T](mat: List[List[T]]) = row >= mat.length
  def index[T](mat: List[List[T]]) = mat(row)(col)

case class Span(val start: Position, val end: Position)
