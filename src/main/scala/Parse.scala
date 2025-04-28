class Parser[T, V <: Visitor[Json]](tokenizer: Tokenizer):
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
      obj <- items
        .foldLeft(
          Right(Map.empty): Either[Tokenizer.TokenError, Map[String, Json]]
        ) { (either_map, either_item) =>
          for {
            map <- either_map
            item <- either_item
          } yield map + item
        }
    } yield Json.ValueObject(obj)

  def parseObjectItem(visitor: V) =
    for {
      keySource <- parseString(visitor)
      key <- keySource.asString.toRight(Tokenizer.Unreachable())
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
      arr <- items
        .foldLeft(
          Right(Seq.empty): Either[Tokenizer.TokenError, Seq[Json]]
        ) { (either_seq, either_item) =>
          for {
            list <- either_seq
            item <- either_item
          } yield list :+ item
        }
    } yield Json.ValueArray(arr)

  def parseString(visitor: V) =
    for {
      stringToken <- tokenizer.tokenize[StringToken]()
    } yield Json.ValueString(stringToken.token.content)

  def parseNull(visitor: V) =
    for {
      nullToken <- tokenizer.tokenize[NullToken]()
    } yield Json.ValueNull

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )
