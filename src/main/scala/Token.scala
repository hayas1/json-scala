import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*

sealed trait Token:
  def repr: String
sealed trait Factory[T <: Token]:
  def tokenize(tokenizer: Tokenizer): Either[TokenizeError, Spanned[T]]

abstract class ControlToken(val represent: Char) extends Token:
  def repr = represent.toString()
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

  // TODO Only this implementation does not advance the cursor to next
  def tokenize(tokenizer: Tokenizer) =
    tokenizer
      .lookAhead()
      .map {
        _ match
          case LEFT_BRACE          => Right(LeftBrace)
          case RIGHT_BRACE         => Right(RightBrace)
          case COLON               => Right(Colon)
          case LEFT_BRACKET        => Right(LeftBracket)
          case RIGHT_BRACKET       => Right(RightBracket)
          case COMMA               => Right(Comma)
          case StringFactory.QUOTE => Right(StringFactory.Quote)
          case NullFactory.NULL0   => Right(NullFactory.Null0)
          case c                   => Left(c)
      } match // TODO how to use cats in this case ?
      case Spanned(Right(t), pos) => Right(Spanned(t, pos))
      case Spanned(Left(c), pos) =>
        Left(Spanned(c, pos)).left.map(TokenizeError.UnknownControl.apply)

case class StringToken(
    startQuote: StringFactory.Quote.type,
    content: String,
    endQuote: StringFactory.Quote.type
) extends Token:
  def repr = startQuote.repr + content + endQuote.repr
given StringFactory: Factory[StringToken] with
  val QUOTE = '"'
  case object Quote extends ControlToken(QUOTE)
  def tokenize(tokenizer: Tokenizer) =
    tokenizer.scope { tz =>
      for {
        start <- tz.expect(Quote)
        content <- tz.tokenizeCharacters()
        end <- tz.expect(Quote)
      } yield StringToken(start.target, content, end.target)
    }.sequence

case class NullToken() extends Token:
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
    tokenizer.scope { tz =>
      for {
        n <- tokenizer.expect(Null0)
        u <- tokenizer.expect(Null1)
        l <- tokenizer.expect(Null2)
        l <- tokenizer.expect(Null3)
      } yield NullToken()
    }.sequence

class Tokenizer(cursor: RowColIterator):
  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while cursor.hasNext && cursor.peek.isWhitespace do cursor.next()
    cursor.position

  def scope[T](f: Tokenizer => T) =
    val start = dropWhitespace()
    val result = f(this)
    val end = cursor.position
    Spanned(result, Span(start, end))

  def expect[T <: ControlToken](token: T, next: Boolean = true) =
    val span = dropWhitespace().asSpan
    val actual = if next then cursor.next() else cursor.peek
    Either.cond(
      token.represent == actual,
      Spanned(token, span),
      TokenizeError.UnexpectedToken(token, Spanned(actual, span))
    )

  def lookAhead() =
    val span = dropWhitespace().asSpan
    Spanned(cursor.peek, span)

  def tokenize[T <: Token]()(using Factory[T]) =
    summon[Factory[T]].tokenize(this)

  def noTrailingPunctuator(punctuator: ControlToken, terminator: ControlToken) =
    val separated = expect(punctuator, false).isRight
    if separated then for { p <- expect(punctuator) } yield p
    else for { t <- expect(terminator, false) } yield t

  def tokenizeCharacters() =
    var builder = new StringBuilder()
    var escaped = false // TODO escape
    // TODO expect will skip whitespace
    while !escaped && expect(StringFactory.Quote, false).isLeft do
      // TODO next=true
      builder.append(cursor.next())
    Right(builder.mkString)

sealed trait TokenizeError extends ParseError

object TokenizeError:
  case class UnexpectedToken(exp: ControlToken, act: Spanned[Char])
      extends TokenizeError:
    def message =
      f"${act.span}: expected '${exp.repr}', but got '${act.target}'"
  case class UnknownControl(act: Spanned[Char]) extends TokenizeError:
    def message = f"${act.span}: unknown control token '${act.target}'"
