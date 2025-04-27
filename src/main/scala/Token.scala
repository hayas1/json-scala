sealed trait Token:
  def span: Span
  def repr: String
sealed trait Factory[T]:
  def tokenize(tokenizer: Tokenizer): Either[Tokenizer.TokenError, T]

abstract class ControlToken(val repr: Char)
given ControlFactory: Factory[ControlToken] with
  val LEFT_BRACE = '{'
  val RIGHT_BRACE = '}'
  val COLON = ':'
  val LEFT_BRACKET = '['
  val RIGHT_BRACKET = ']'
  val COMMA = ','

  case object LeftBrace extends ControlToken(LEFT_BRACE)
  case object RightBrace extends ControlToken(RIGHT_BRACE)
  case object LeftBracket extends ControlToken(LEFT_BRACKET)
  case object RightBracket extends ControlToken(RIGHT_BRACKET)
  case object Colon extends ControlToken(COLON)
  case object Comma extends ControlToken(COMMA)

  // TODO only this implementation has no side effects
  def tokenize(tokenizer: Tokenizer) = tokenizer.lookAhead() match
    case Spanned(LEFT_BRACE, _)          => Right(LeftBrace)
    case Spanned(RIGHT_BRACE, _)         => Right(RightBrace)
    case Spanned(COLON, _)               => Right(Colon)
    case Spanned(LEFT_BRACKET, _)        => Right(LeftBracket)
    case Spanned(RIGHT_BRACKET, _)       => Right(RightBracket)
    case Spanned(COMMA, _)               => Right(Comma)
    case Spanned(StringFactory.QUOTE, _) => Right(StringFactory.Quote)
    case Spanned(NullFactory.NULL0, _)   => Right(NullFactory.Null0)
    case Spanned(char, p) => Left(Tokenizer.UnknownControl(Spanned(char, p)))

case class StringToken(
    startQuote: Spanned[StringFactory.Quote.type],
    content: String,
    endQuote: Spanned[StringFactory.Quote.type]
) extends Token:
  def span = startQuote.span.merged(endQuote.span)
  def repr = startQuote.token.repr + content + endQuote.token.repr
given StringFactory: Factory[StringToken] with
  val QUOTE = '"'
  case object Quote extends ControlToken(QUOTE)
  def tokenize(tokenizer: Tokenizer) =
    for {
      start <- tokenizer.expect(Quote)
      content <- tokenizer.tokenizeCharacters()
      end <- tokenizer.expect(Quote)
    } yield StringToken(start, content, end)

case class NullToken(span: Span) extends Token:
  def repr = List(
    NullFactory.NULL0,
    NullFactory.NULL1,
    NullFactory.NULL2,
    NullFactory.NULL3
  ).mkString
given NullFactory: Factory[NullToken] with
  val NULL0 = 'n'
  val NULL1 = 'u'
  val NULL2 = 'l'
  val NULL3 = 'l'
  case object Null0 extends ControlToken(NULL0)
  case object Null1 extends ControlToken(NULL1)
  case object Null2 extends ControlToken(NULL2)
  case object Null3 extends ControlToken(NULL3)
  def tokenize(tokenizer: Tokenizer) =
    val start = tokenizer.dropWhitespace()
    for {
      n <- tokenizer.expect(Null0)
      u <- tokenizer.expect(Null1)
      l <- tokenizer.expect(Null2)
      l <- tokenizer.expect(Null3)
      end = tokenizer.position
    } yield NullToken(Span(start, end))

class Tokenizer(cursor: RowColIterator):
  def position = cursor.position
  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while cursor.hasNext && cursor.peek.isWhitespace do cursor.next()
    cursor.position

  def expect[T <: ControlToken](token: T, next: Boolean = true) =
    val span = dropWhitespace().asSpan
    val actual = if next then cursor.next() else cursor.peek
    Either.cond(
      token.repr == actual,
      Spanned(token, span),
      Tokenizer.UnexpectedToken(token, Spanned(actual, span))
    )

  def lookAhead() =
    val span = dropWhitespace().asSpan
    Spanned(cursor.peek, span)

  def tokenize[T]()(using Factory[T]) =
    summon[Factory[T]].tokenize(this)

  def punctuated[T](punctuator: ControlToken, terminator: ControlToken)(
      f: => T
  ) =
    var seq = Seq[T]()
    var separated = true
    while separated && expect(terminator, false).isLeft do
      seq = seq :+ f // TODO early return
      separated = expect(punctuator, false).isRight
      if separated then for { _ <- expect(punctuator) } yield ()
    // TODO trailing commas
    for { _ <- expect(terminator) } yield seq

  def tokenizeCharacters() =
    var builder = new StringBuilder()
    var escaped = false // TODO escape
    while !escaped && expect(StringFactory.Quote, false).isLeft do
      // TODO next=true
      builder.append(cursor.next())
    Right(builder.mkString)

object Tokenizer:
  sealed trait TokenError:
    def message: String
    override def toString() = message
  case class UnexpectedToken(exp: ControlToken, act: Spanned[Char])
      extends TokenError:
    def message = f"${act.span}: expected '${exp.repr}', but got '${act.token}'"
  case class Unreachable(msg: String = "unreachable") extends TokenError:
    def message = msg
  case class UnknownControl(act: Spanned[Char]) extends TokenError:
    def message = f"${act.span}: unknown control token '${act.token}'"

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

case class Span(val start: Position, val end: Position):
  override def toString() =
    val (start1, end1) = (start.oneIndexed, end.oneIndexed)
    if start1 == end1 then s"row ${start1.row}, col ${start1.col}"
    else s"(${start1.row}, ${start1.col}) to (${end1.row}, ${end1.col})"
  def merged(other: Span) = Span(
    summon[Ordering[Position]].min(start, other.start),
    summon[Ordering[Position]].max(end, other.end)
  )
  def spanned[T](token: T) = Spanned(token, this)

case class Spanned[T](val token: T, val span: Span)
