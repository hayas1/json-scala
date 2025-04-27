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
  def nextChar() = cursor.next()

  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while cursor.hasNext && cursor.peek.isWhitespace do cursor.next()
    cursor

  def expect(token: ControlToken, next: Boolean = true) =
    val actual = if next then dropWhitespace().next() else dropWhitespace().peek
    Either.cond(
      ControlToken(actual) == Some(token),
      actual,
      Tokenizer.UnexpectedToken(token, actual)
    )

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

  def tokenize_string_content() =
    var builder = new StringBuilder()
    var escaped = false // TODO escape
    while !escaped && expect(ControlToken.Quote, false).isLeft do
      // TODO next=true
      builder.append(nextChar())
    Right(builder.mkString)

object Tokenizer:
  def apply(target: String) =
    new Tokenizer(
      new RowColIterator(target.linesIterator.toList.map(_.toList))
    )

  sealed trait TokenError:
    def message: String
    override def toString() = message
  case class UnexpectedToken(exp: ControlToken, act: Char) extends TokenError:
    def message = f"expected '${exp.represent}', but got '$act'"
  case class Unreachable(msg: String = "unreachable") extends TokenError:
    def message = msg

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
