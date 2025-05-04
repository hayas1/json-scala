import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*

sealed trait Token:
  def repr: String
sealed trait Factory[T <: Token]:
  def tokenize(tokenizer: Tokenizer): Either[TokenizeError, T]

abstract class ControlToken(val represent: Char) extends Token:
  def repr = represent.toString
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
      .lookAhead() {
        case LEFT_BRACE          => Right(LeftBrace)
        case RIGHT_BRACE         => Right(RightBrace)
        case COLON               => Right(Colon)
        case LEFT_BRACKET        => Right(LeftBracket)
        case RIGHT_BRACKET       => Right(RightBracket)
        case COMMA               => Right(Comma)
        case StringFactory.QUOTE => Right(StringFactory.Quote)
        case NumberFactory.PLUS  => Right(NumberFactory.Plus)
        case NumberFactory.MINUS => Right(NumberFactory.Minus)
        case d if NumberFactory.DIGIT contains d =>
          Right(NumberFactory.DigitTokens(d - '0'))
        case NumberFactory.DOT => Right(NumberFactory.Dot)
        case NumberFactory.EXPONENT_SMALL =>
          Right(NumberFactory.ExponentSmall)
        case NumberFactory.EXPONENT_LARGE =>
          Right(NumberFactory.ExponentLarge)
        case NullFactory.NULL0 => Right(NullFactory.Null0)
        case c                 => Left(c)
      }
      .left
      .map(TokenizeError.UnknownControl(_))

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
    for {
      start <- tokenizer.expect(Quote)
      content <- tokenizer.tokenizeCharacters()
      end <- tokenizer.expect(Quote)
    } yield StringToken(start, content, end)

case class NumberToken(
    sign: Option[NumberFactory.Minus.type],
    digits: NumberFactory.Digits,
    fraction: Option[NumberFactory.Fraction],
    exponent: Option[NumberFactory.Exponent]
) extends Token:
  def repr =
    val signStr = sign.map(_.repr).mkString
    val digitsStr = digits.map(_.repr).mkString
    val fractionStr = fraction.map { (dot, digits) =>
      dot.repr + digits.map(_.repr).mkString
    }.mkString
    val exponentStr = exponent.map { (exp, sign, digits) =>
      exp.repr + sign.map(_.repr).mkString + digits.map(_.repr).mkString
    }.mkString
    s"$signStr$digitsStr$fractionStr$exponentStr"

given NumberFactory: Factory[NumberToken] with
  val PLUS = '+'
  val MINUS = '-'
  val DIGIT = '0' to '9'
  val DOT = '.'
  val EXPONENT_SMALL = 'e'
  val EXPONENT_LARGE = 'E'
  case object Plus extends ControlToken(PLUS)
  case object Minus extends ControlToken(MINUS)
  case object Digit0 extends ControlToken(DIGIT(0))
  case object Digit1 extends ControlToken(DIGIT(1))
  case object Digit2 extends ControlToken(DIGIT(2))
  case object Digit3 extends ControlToken(DIGIT(3))
  case object Digit4 extends ControlToken(DIGIT(4))
  case object Digit5 extends ControlToken(DIGIT(5))
  case object Digit6 extends ControlToken(DIGIT(6))
  case object Digit7 extends ControlToken(DIGIT(7))
  case object Digit8 extends ControlToken(DIGIT(8))
  case object Digit9 extends ControlToken(DIGIT(9))
  case object Dot extends ControlToken(DOT)
  case object ExponentSmall extends ControlToken(EXPONENT_SMALL)
  case object ExponentLarge extends ControlToken(EXPONENT_LARGE)

  val DigitTokens = List(
    Digit0,
    Digit1,
    Digit2,
    Digit3,
    Digit4,
    Digit5,
    Digit6,
    Digit7,
    Digit8,
    Digit9
  )

  type Sign = Plus.type | Minus.type
  type Digit = Digit0.type | Digit1.type | Digit2.type | Digit3.type |
    Digit4.type | Digit5.type | Digit6.type | Digit7.type | Digit8.type |
    Digit9.type
  type Digits = Seq[Digit]
  type Fraction = (Dot.type, Digits)
  type Exponent =
    (ExponentSmall.type | ExponentLarge.type, Option[Sign], Digits)

  def tokenize(tokenizer: Tokenizer) =
    val sign = tokenizer.expect(Minus, false).toOption
    for {
      digits <- tokenizer.tokenizeDigits()
      dot = tokenizer.expect(Dot, false).toOption
      fraction = for {
        d <- dot
        dotToken <- tokenizer.expect(d).toOption // TODO left
        digits <- tokenizer.tokenizeDigits().toOption
      } yield (d, digits)
      exp = tokenizer.expect(ExponentSmall, false).toOption orElse
        tokenizer.expect(ExponentLarge, false).toOption
      exponent = for {
        e <- exp
        expToken <- tokenizer.expect(e, false).toOption // TODO left
        sign = tokenizer.expect(Minus, false).toOption orElse
          tokenizer.expect(Plus, false).toOption
        digits <- tokenizer.tokenizeDigits().toOption
      } yield (
        expToken.asInstanceOf[ExponentSmall.type | ExponentLarge.type],
        sign.asInstanceOf[Option[Sign]],
        digits
      )
    } yield NumberToken(sign, digits, fraction, exponent)

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
    for {
      n <- tokenizer.expect(Null0)
      u <- tokenizer.expect(Null1)
      l <- tokenizer.expect(Null2)
      l <- tokenizer.expect(Null3)
    } yield NullToken()

class Tokenizer(cursor: RowColIterator):
  def dropWhitespace() =
    // TODO peek after Iterator.dropWhile(_.isWhitespace) will return whitespace
    while cursor.hasNext && cursor.peek.isWhitespace do cursor.next()
    cursor.position

  def scope[C, T](context: C)(f: => T) =
    val start = dropWhitespace()
    val result = f
    val end = cursor.position
    (Spanned(context, Span(start, end)), result)

  def expect[T <: ControlToken](token: T, next: Boolean = true) =
    val span = dropWhitespace().asSpan
    val actual = if next then cursor.next() else cursor.peek
    Either.cond(
      token.represent == actual,
      token,
      TokenizeError.UnexpectedToken(token, Spanned(actual, span))
    )

  def lookAhead[A, B](drop: Boolean = true)(f: Char => Either[A, B]) =
    val span = (if drop then dropWhitespace() else cursor.position).asSpan
    f(cursor.peek).left.map(Spanned(_, span))

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

  def tokenizeDigits() =
    var builder: NumberFactory.Digits = Seq.empty
    var look = lookAhead(false) { // TODO leading zeros
      case c if NumberFactory.DIGIT contains c =>
        Right(NumberFactory.DigitTokens(c - '0'))
      case _ => Left(())
    }

    while look.isRight do
      val digitToken = for {
        digit <- look
        nextToken <- expect(digit)
      } yield nextToken match {
        case d: NumberFactory.Digit => d
      } // TODO left
      builder = builder :+ digitToken.getOrElse(throw new NotImplementedError)
      look = lookAhead(false) {
        case c if NumberFactory.DIGIT contains c =>
          Right(NumberFactory.DigitTokens(c - '0'))
        case _ => Left(())
      }
    Right(builder)

sealed trait TokenizeError extends ParseError
object TokenizeError:
  case class UnexpectedToken(exp: ControlToken, act: Spanned[Char])
      extends TokenizeError:
    override def span = Some(act.span)
    def message = f"expected '${exp.repr}', but got '${act.target}'"
  case class UnknownControl(act: Spanned[Char]) extends TokenizeError:
    override def span = Some(act.span)
    def message = f"unknown control token '${act.target}'"
  case class NotFoundToken() extends TokenizeError:
    def message = "not found token"
