import cats.syntax.functor.*
import cats.syntax.traverse.*

class Parser[V <: Visitor](tokenizer: Tokenizer):
  def parseValue[T](using
      visitor: V[T],
      kv: Visitor[visitor.Key],
      vv: Visitor[visitor.Value]
  ): Either[Tokenizer.TokenError, T] =
    tokenizer.tokenize[ControlToken]() match
      case Right(Spanned(ControlFactory.LeftBrace, _))   => parseObject
      case Right(Spanned(ControlFactory.LeftBracket, _)) => parseArray
      case Right(Spanned(StringFactory.Quote, _))        => parseString
      case Right(Spanned(NullFactory.Null0, _))          => parseNull
      case _ => throw new NotImplementedError

  def parseObject[T](using
      visitor: V[T]
  )(using kv: Visitor[visitor.Key], vv: Visitor[visitor.Value]) =
    val objectParser = new Parser(tokenizer)
    for {
      leftBrace <- tokenizer.expect(ControlFactory.LeftBrace)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBrace) {
          for {
            key <- objectParser.parseString[visitor.Key]
            colon <- tokenizer.expect(ControlFactory.Colon)
            value <- objectParser.parseValue[visitor.Value](using vv)
          } yield (key, value)
        }
      obj <- items.sequence
    } yield visitor.visitObject(obj)

  def parseArray[T](using visitor: V[T], vv: Visitor[visitor.Value]) =
    val arrayParser = new Parser(tokenizer)
    for {
      leftBracket <- tokenizer.expect(ControlFactory.LeftBracket)
      items <- tokenizer
        .punctuated(ControlFactory.Comma, ControlFactory.RightBracket) {
          arrayParser.parseValue[visitor.Value]
        }
      arr <- items.sequence
    } yield visitor.visitArray(arr)

  def parseString[T](using visitor: V[T]) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
    } yield visitor.visitString(stringToken.token.content)

  def parseNull[T](using visitor: V[T]) =
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
