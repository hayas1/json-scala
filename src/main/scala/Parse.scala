import cats.syntax.functor.*
import cats.syntax.traverse.*

class Parser[T, V <: Visitor[Json]](tokenizer: Tokenizer): // TODO Json -> Json
  def parseValue(visitor: V): Either[Tokenizer.TokenError, Json] =
    tokenizer.tokenize[ControlToken]() match
      case Right(Spanned(ControlFactory.LeftBrace, _))   => parseObject(visitor)
      case Right(Spanned(ControlFactory.LeftBracket, _)) => parseArray(visitor)
      case Right(Spanned(StringFactory.Quote, _))        => parseString(visitor)
      case Right(Spanned(NullFactory.Null0, _))          => parseNull(visitor)
      case _ => throw new NotImplementedError

  def parseObject(visitor: V) =
    for {
      leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBrace) {
          parseObjectItem(visitor)
        }
      obj <- items.sequence
    } yield visitor.visit_object(obj)

  def parseObjectItem(visitor: V) =
    for {
      key <- parseString(visitor)
      colon <- tokenizer.expect(ControlFactory.Colon)
      value <- parseValue(visitor)
    } yield (key, value)

  def parseArray(visitor: V) =
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBracket) {
          parseValue(visitor)
        }
      arr <- items.sequence
    } yield visitor.visit_array(arr)

  def parseString(visitor: V) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
    } yield visitor.visit_string(stringToken.token.content)

  def parseNull(visitor: V) =
    for {
      nullToken <- tokenizer.tokenize[NullToken]()
    } yield visitor.visit_null()

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )
