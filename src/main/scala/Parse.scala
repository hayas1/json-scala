import cats.syntax.functor.*
import cats.syntax.traverse.*

class Parser(tokenizer: Tokenizer):
  def parseValue[T, V <: Visitor](
      visitor: V[T]
  ): Either[Tokenizer.TokenError, T] =
    tokenizer.tokenize[ControlToken]() match
      case Right(Spanned(ControlFactory.LeftBrace, _))   => parseObject(visitor)
      case Right(Spanned(ControlFactory.LeftBracket, _)) => parseArray(visitor)
      case Right(Spanned(StringFactory.Quote, _))        => parseString(visitor)
      case Right(Spanned(NullFactory.Null0, _))          => parseNull(visitor)
      case _ => throw new NotImplementedError

  def parseObject[T, V <: Visitor](visitor: V[T]) =
    val objectAccessor = new ObjectAccessor(this)
    for {
      leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBrace) {
          for {
            key <- parseString(visitor)
            colon <- tokenizer.expect(ControlFactory.Colon)
            value <- parseValue(visitor)
          } yield (key, value)
        }
      obj <- items.sequence
    } yield visitor.visitObject(objectAccessor)

  def parseArray[T, V <: Visitor](visitor: V[T]) =
    val arrayAccessor = new ArrayAccessor(this)
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBracket) {
          parseValue(visitor)
        }
      arr <- items.sequence
    } yield visitor.visitArray(arrayAccessor)

  def parseString[T, V <: Visitor](visitor: V[T]) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
    } yield visitor.visitString(stringToken.token.content)

  def parseNull[T, V <: Visitor](visitor: V[T]) =
    for {
      nullToken <- tokenizer.tokenize[NullToken]()
    } yield visitor.visitNull()

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )
