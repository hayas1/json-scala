import cats.syntax.functor.*
import cats.syntax.traverse.*

def parseJson[T](input: String)(using visitor: Visitor[T]) =
  Parser(input).parseValue()

class Parser(val tokenizer: Tokenizer):
  def parseValue[T, V <: Visitor]()(using
      visitor: V[T]
  ): Either[ParseError, T] =
    val (_, value) = tokenizer.scope(()) {
      for {
        control <- tokenizer.tokenize[ControlToken]()
        value <- control match
          case ControlFactory.LeftBrace   => parseObject()
          case ControlFactory.LeftBracket => parseArray()
          case StringFactory.Quote        => parseString()
          case NumberFactory.Plus | NumberFactory.Minus | NumberFactory.Digit0 |
              NumberFactory.Digit1 | NumberFactory.Digit2 |
              NumberFactory.Digit3 | NumberFactory.Digit4 |
              NumberFactory.Digit5 | NumberFactory.Digit6 |
              NumberFactory.Digit7 | NumberFactory.Digit8 |
              NumberFactory.Digit9 =>
            parseNumber()
          case BoolFactory.True0 | BoolFactory.False0 => parseBool()
          case NullFactory.Null0                      => parseNull()
          case token =>
            Left(
              TokenizeError.UnknownControl( // TODO get Span
                Spanned(token.represent, Span(Position(), Position()))
              )
            )
      } yield value
    }
    value.left.map(ParseError.WhileParsing(_))

  def parseObject[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, members) = tokenizer.scope(ValueType.Object) {
      for {
        leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
        members <- visitor.visitObject(ObjectAccessor(this))
        rightBrace <- tokenizer.expect(ControlFactory.RightBrace)
      } yield members
    }
    members.left.map(ParseError.Context(ctx, _))

  def parseArray[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, elements) = tokenizer.scope(ValueType.Array) {
      for {
        leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
        elements <- visitor.visitArray(ArrayAccessor(this))
        rightBracket <- tokenizer.expect(ControlFactory.RightBracket)
      } yield elements
    }
    elements.left.map(ParseError.Context(ctx, _))

  def parseString[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, string) = tokenizer.scope(ValueType.String) {
      for {
        stringToken <- tokenizer.tokenize[StringToken]()
        string <- visitor.visitString(stringToken.content)
      } yield string
    }
    string.left.map(ParseError.Context(ctx, _))

  def parseNumber[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, number) = tokenizer.scope(ValueType.Number) {
      for {
        numberToken <- tokenizer.tokenize[NumberToken]()
        number <- visitor.visitNumber(numberToken.repr.toDouble) // TODO token?
      } yield number
    }
    number.left.map(ParseError.Context(ctx, _))

  def parseBool[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, bool) = tokenizer.scope(ValueType.Bool) {
      for {
        boolToken <- tokenizer.tokenize[BoolToken]()
        bool <- visitor.visitBool(boolToken.bool.isInstanceOf[BoolFactory.True])
      } yield bool
    }
    bool.left.map(ParseError.Context(ctx, _))

  def parseNull[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, nullValue) = tokenizer.scope(ValueType.Null) {
      for {
        nullToken <- tokenizer.tokenize[NullToken]()
        nullValue <- visitor.visitNull()
      } yield nullValue
    }
    nullValue.left.map(ParseError.Context(ctx, _))

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )

class ObjectAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBrace

  def hasNextPair = parser.tokenizer.expect(terminator, false).isLeft
  def nextName[N]()(using visitor: Visitor[N]) =
    val (_, name) = parser.tokenizer.scope(()) {
      for {
        name <- parser.parseString()
        colon <- parser.tokenizer.expect(ControlFactory.Colon)
      } yield name
    }
    name.left.map(ParseError.WhileParsing(_))
  def nextValue[V]()(using visitor: Visitor[V]) =
    val (_, value) = parser.tokenizer.scope(()) {
      for {
        value <- parser.parseValue()
        comma <- parser.tokenizer.noTrailingPunctuator(punctuator, terminator)
      } yield value
    }
    value.left.map(ParseError.WhileParsing(_))

  def pairs[N, V](using nv: Visitor[N], vv: Visitor[V]) =
    new Iterator[Either[ParseError, (N, V)]]:
      def hasNext = hasNextPair
      def next() =
        for {
          name <- nextName[N]()
          value <- nextValue[V]()
        } yield (name, value)

class ArrayAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBracket

  def hasNextElement = parser.tokenizer.expect(terminator, false).isLeft
  def nextElement[V]()(using visitor: Visitor[V]) =
    val (_, element) = parser.tokenizer.scope(()) {
      for {
        element <- parser.parseValue()
        comma <- parser.tokenizer.noTrailingPunctuator(punctuator, terminator)
      } yield element
    }
    element.left.map(ParseError.WhileParsing(_))

  def elements[V](using vv: Visitor[V]) = new Iterator[Either[ParseError, V]]:
    def hasNext = hasNextElement
    def next() = nextElement()

trait ParseError extends Throwable: // TODO refactor error handling
  def span: Option[Span] = None
  def cause: Option[ParseError] = None
  def message: String
  override def toString = message
object ParseError:
  case class Context[E <: ParseError](context: Spanned[ValueType], e: E)
      extends ParseError:
    override def span = Some(context.span)
    override def cause = Some(e)
    def message =
      var rep: ParseError = this
      var s: Option[Span] = span
      while rep.cause.nonEmpty do
        rep = rep.cause.get
        s = rep.span orElse s
      s"${s getOrElse ""}: ${e.message}"
  case class WhileParsing[E <: ParseError](e: E) extends ParseError:
    override def span = None
    override def cause = Some(e)
    def message = s"${e.message}"
