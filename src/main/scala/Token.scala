import Tokenizer.UnexpectedToken
abstract class ControlToken(val represent: Char)
object ControlToken:
  val LEFT_BRACE = '{'
  val RIGHT_BRACE = '}'
  val COLON = ':'
  val COMMA = ','
  val QUOTE = '"'

  case object LeftBrace extends ControlToken(ControlToken.LEFT_BRACE)
  case object RightBrace extends ControlToken(ControlToken.RIGHT_BRACE)
  case object Colon extends ControlToken(ControlToken.COLON)
  case object Comma extends ControlToken(ControlToken.COMMA)
  case object Quote extends ControlToken(ControlToken.QUOTE)

  def apply(represent: Char) = represent match
    case LEFT_BRACE  => Some(LeftBrace)
    case RIGHT_BRACE => Some(RightBrace)
    case COLON       => Some(Colon)
    case COMMA       => Some(Comma)
    case QUOTE       => Some(Quote)
    case _           => None

class Tokenizer(cursor: RowColIterator):
  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while cursor.hasNext && cursor.peek.isWhitespace do cursor.next()
    cursor.position

  def expect(token: ControlToken, next: Boolean = true) =
    val pos = dropWhitespace()
    val actual =
      Spanned(if next then cursor.next() else cursor.peek, Span(pos, pos))
    Either.cond(
      ControlToken(actual.token) == Some(token),
      actual,
      Tokenizer.UnexpectedToken(token, actual)
    )

  def lookAhead() =
    dropWhitespace()
    ControlToken(cursor.peek)

  def punctuated[T](punctuator: ControlToken, terminator: ControlToken)(
      f: => T
  ) =
    var seq = Seq[T]()
    var separated = true
    while separated && expect(terminator, false).isLeft do
      seq = seq :+ f
      separated = expect(punctuator, false).isRight
    for {
      _ <- expect(terminator)
    } yield seq

  def tokenizeStringContent() =
    var builder = new StringBuilder()
    var escaped = false // TODO escape
    while !escaped && expect(ControlToken.Quote, false).isLeft do
      // TODO next=true
      builder.append(cursor.next())
    Right(builder.mkString)

object Tokenizer:
  def apply(target: String) =
    new Tokenizer(
      new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
    )

  sealed trait TokenError:
    def message: String
    override def toString() = message
  case class UnexpectedToken(exp: ControlToken, act: Spanned[Char])
      extends TokenError:
    def message =
      f"${act.span}: expected '${exp.represent}', but got '${act.token}'"
  case class Unreachable(msg: String = "unreachable") extends TokenError:
    def message = msg

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

case class Span(val start: Position, val end: Position):
  override def toString() =
    val (start1, end1) = (start.oneIndexed, end.oneIndexed)
    if start1 == end1 then s"row ${start1.row}, col ${start1.col}"
    else s"(${start1.row}, ${start1.col}) to (${end1.row}, ${end1.col})"

case class Spanned[T](val token: T, val span: Span)
