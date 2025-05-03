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
          case NullFactory.Null0          => parseNull()
          case token => Left(TokenizeError.UnknownControl(token.represent))
      } yield value
    }
    value.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError.WhileParsing(e)
      case e: ParseError             => ParseError.WhileParsing(e)
    }

  def parseObject[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, members) = tokenizer.scope(ValueType.Object) {
      for {
        leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
        members <- visitor.visitObject(ObjectAccessor(this))
        rightBrace <- tokenizer.expect(ControlFactory.RightBrace)
      } yield members
    }
    // members.left.map(ParseError(ctx, _))
    members.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError(ctx, e)
      case e: VisitorError           => ParseError(ctx, e)
    }

  def parseArray[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, elements) = tokenizer.scope(ValueType.Array) {
      for {
        leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
        elements <- visitor.visitArray(ArrayAccessor(this))
        rightBracket <- tokenizer.expect(ControlFactory.RightBracket)
      } yield elements
    }
    elements.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError(ctx, e)
      case e: VisitorError           => ParseError(ctx, e)
    }

  def parseString[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, string) = tokenizer.scope(ValueType.String) {
      for {
        stringToken <- tokenizer.tokenize[StringToken]()
        string <- visitor.visitString(stringToken.content)
      } yield string
    }
    string.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError(ctx, e)
      case e: VisitorError           => ParseError(ctx, e)
    }

  def parseNull[T, V <: Visitor]()(using visitor: V[T]) =
    val (ctx, nullValue) = tokenizer.scope(ValueType.Null) {
      for {
        nullToken <- tokenizer.tokenize[NullToken]()
        nullValue <- visitor.visitNull()
      } yield nullValue
    }
    nullValue.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError(ctx, e)
      case e: VisitorError           => ParseError(ctx, e)
    }

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )

trait ParseError extends Throwable:
  def span: Option[Span] = None
  def cause: Option[ParseError] = None
  def message: String
  override def toString = message
object ParseError:
  def apply[E <: ParseError](context: Spanned[ValueType], e: E | Spanned[E]) =
    e match
      case e: Spanned[E] => ParseError.ContextSpanned(context, e)
      case e: ParseError => ParseError.Context(context, e)

  case class Context[E <: ParseError](context: Spanned[ValueType], e: E)
      extends ParseError:
    override def span = Some(context.span)
    override def cause = Some(e)
    def message = s"$span: ${e.message}"
  case class ContextSpanned[E <: ParseError](
      context: Spanned[ValueType],
      e: Spanned[E]
  ) extends ParseError:
    override def span = Some(context.span)
    override def cause = Some(e.target)
    def message = s"$span: ${e.target.message}"
  case class WhileParsing[E <: ParseError](e: E | Spanned[E])
      extends ParseError:
    override def span = e match
      case e: Spanned[E] => Some(e.span)
      case e: ParseError => None

    override def cause = e match
      case e: Spanned[E] => Some(e.target)
      case e: ParseError => Some(e)
    def message = e match
      case e: Spanned[E] => s"$span: ${e.target.message}"
      case e: ParseError => e.message

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
    name.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError.WhileParsing(e)
      case e: ParseError             => ParseError.WhileParsing(e)
    }
  def nextValue[V]()(using visitor: Visitor[V]) =
    val (_, value) = parser.tokenizer.scope(()) {
      for {
        value <- parser.parseValue()
        comma <- parser.tokenizer.noTrailingPunctuator(punctuator, terminator)
      } yield value
    }
    value.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError.WhileParsing(e)
      case e: ParseError             => ParseError.WhileParsing(e)
    }

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
    element.left.map { // TODO use apply
      case e: Spanned[TokenizeError] => ParseError.WhileParsing(e)
      case e: ParseError             => ParseError.WhileParsing(e)
    }

  def elements[V](using vv: Visitor[V]) = new Iterator[Either[ParseError, V]]:
    def hasNext = hasNextElement
    def next() = nextElement()
