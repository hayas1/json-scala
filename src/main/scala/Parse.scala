import cats.syntax.functor.*
import cats.syntax.traverse.*

class Parser[V <: Visitor](tokenizer: Tokenizer):
  def parseValue[T](visitor: V[T]): Either[Tokenizer.TokenError, T] =
    tokenizer.tokenize[ControlToken]() match
      case Right(Spanned(ControlFactory.LeftBrace, _))   => parseObject(visitor)
      case Right(Spanned(ControlFactory.LeftBracket, _)) => parseArray(visitor)
      case Right(Spanned(StringFactory.Quote, _))        => parseString(visitor)
      case Right(Spanned(NullFactory.Null0, _))          => parseNull(visitor)
      case _ => throw new NotImplementedError

  def parseObject[T](
      visitor: V[T]
  )(using kv: Visitor[visitor.Key], vv: Visitor[visitor.Value]) =
    val objectParser = new Parser(tokenizer)
    for {
      leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBrace) {
          for {
            key <- objectParser.parseString(kv)
            colon <- tokenizer.expect(ControlFactory.Colon)
            value <- objectParser.parseValue(vv)
          } yield (key, value)
        }
      obj <- items.sequence
    } yield visitor.visit_object(obj)

  def parseArray[T](visitor: V[T])(using vv: Visitor[visitor.Value]) =
    val arrayParser = new Parser(tokenizer)
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBracket) {
          arrayParser.parseValue(vv)
        }
      arr <- items.sequence
    } yield visitor.visit_array(arr)

  def parseString[T](visitor: V[T]) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
    } yield visitor.visit_string(stringToken.token.content)

  def parseNull[T](visitor: V[T]) =
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
