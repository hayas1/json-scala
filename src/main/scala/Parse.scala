import cats.syntax.functor.*
import cats.syntax.traverse.*

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
      obj <- visitor.visitObject(ObjectAccessor(this))
      rightBrace <- tokenizer.expect(ControlFactory.RightBrace)
    } yield obj

  def parseArray[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      arr <- visitor.visitArray(ArrayAccessor(this))
      rightBracket <- tokenizer.expect(ControlFactory.RightBracket)
    } yield arr

  def parseString[T, V <: Visitor]()(using visitor: V[T]) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
      string <- visitor.visitString(stringToken.token.content)
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

trait ParseError extends Throwable

class ObjectAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBrace

  def hasNextItem = parser.tokenizer.expect(terminator, false).isLeft
  def nextKey[K](using visitor: Visitor[K]) = for {
    key <- parser.parseString()
    colon <- parser.tokenizer.expect(ControlFactory.Colon)
  } yield key
  def nextValue[V](using visitor: Visitor[V]) = for {
    value <- parser.parseValue()
    comma <- parser.tokenizer.trailingPunctuator(punctuator, terminator)
  } yield value

  // TODO toMap, toList, toSeq, etc ?
  def toIter[K, V](using kv: Visitor[K], vv: Visitor[V]) = new Iterator[(K, V)]:
    def hasNext = hasNextItem
    def next() = (for {
      key <- nextKey[K]
      value <- nextValue[V]
    } yield (key, value)).getOrElse(throw new RuntimeException)

class ArrayAccessor(parser: Parser):
  def punctuator = ControlFactory.Comma
  def terminator = ControlFactory.RightBracket

  def hasNextValue = parser.tokenizer.expect(terminator, false).isLeft
  def nextValue[V](using visitor: Visitor[V]) = for {
    value <- parser.parseValue()
    comma <- parser.tokenizer.trailingPunctuator(punctuator, terminator)
  } yield value

  // TODO toList, toSeq, etc ?
  def toIter[V](using vv: Visitor[V]) = new Iterator[V]:
    def hasNext = hasNextValue
    def next() = nextValue.getOrElse(throw new RuntimeException)
