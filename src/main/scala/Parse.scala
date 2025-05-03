import cats.syntax.functor.*
import cats.syntax.traverse.*

def parseJson[T](input: String)(using visitor: Visitor[T]) =
  Parser(input).parseValue()

class Parser(val tokenizer: Tokenizer):
  def parseValue[T, V <: Visitor]()(using
      visitor: V[T]
  ): Either[ParseError, T] =
    for {
      control <- tokenizer.tokenize[ControlToken]()
      value <- control match
        case Spanned(ControlFactory.LeftBrace, _)   => parseObject()
        case Spanned(ControlFactory.LeftBracket, _) => parseArray()
        case Spanned(StringFactory.Quote, _)        => parseString()
        case Spanned(NullFactory.Null0, _)          => parseNull()
        case Spanned(token, span) =>
          Left(TokenizeError.UnknownControl(Spanned(token.represent, span)))
    } yield value

  def parseObject[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
      members <- visitor.visitObject(ObjectAccessor(this))
      rightBrace <- tokenizer.expect(ControlFactory.RightBrace)
    } yield members

  def parseArray[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      elements <- visitor.visitArray(ArrayAccessor(this))
      rightBracket <- tokenizer.expect(ControlFactory.RightBracket)
    } yield elements

  def parseString[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
      string <- visitor.visitString(stringToken.target.content)
    } yield string

  def parseNull[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      nullToken <- tokenizer.tokenize[NullToken]()
      nullValue <- visitor.visitNull()
    } yield nullValue

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )

trait ParseError extends Throwable:
  def message: String
  override def toString = message
given [T <: ParseError]: Conversion[Spanned[T], ParseError] with
  def apply(spanned: Spanned[T]): ParseError = new ParseError {
    def message: String = s"${spanned.span}: ${spanned.target.message}"
  }

class ObjectAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBrace

  def hasNextPair = parser.tokenizer.expect(terminator, false).isLeft
  def nextName[N]()(using visitor: Visitor[N]) =
    for {
      name <- parser.parseString()
      colon <- parser.tokenizer.expect(ControlFactory.Colon)
    } yield name
  def nextValue[V]()(using visitor: Visitor[V]) =
    for {
      value <- parser.parseValue()
      comma <- parser.tokenizer.noTrailingPunctuator(punctuator, terminator)
    } yield value

  def pairs[N, V](using nv: Visitor[N], vv: Visitor[V]) =
    new Iterator[Either[ParseError, (N, V)]]:
      def hasNext = hasNextPair
      def next() =
        (for { // TODO invalid json such like {"name" "value"}
          name <- nextName[N]()
          value <- nextValue[V]()
        } yield (name, value))

class ArrayAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBracket

  def hasNextElement = parser.tokenizer.expect(terminator, false).isLeft
  def nextElement[V]()(using visitor: Visitor[V]) =
    for {
      element <- parser.parseValue()
      comma <- parser.tokenizer.noTrailingPunctuator(punctuator, terminator)
    } yield element

  def elements[V](using vv: Visitor[V]) = new Iterator[Either[ParseError, V]]:
    def hasNext = hasNextElement
    def next() = nextElement()
