class Parser(tokenizer: Tokenizer):
  def parseValue(): Either[Tokenizer.TokenError, Json] =
    tokenizer.lookAhead() match
      case Some(ControlToken.LeftBrace)   => parseObject()
      case Some(ControlToken.LeftBracket) => parseArray()
      case Some(StringToken.Quote)        => parseString()
      case Some(NullToken.Null0)          => parseNull()
      case _                              => throw new NotImplementedError

  def parseObject() =
    for {
      leftBrace <- tokenizer.expect(ControlToken.LeftBrace)
      items <- tokenizer
        .punctuated(ControlToken.Comma, ControlToken.RightBrace) {
          parseObjectItem()
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

  def parseObjectItem() =
    for {
      keySource <- parseString()
      key <- keySource.asString.toRight(Tokenizer.Unreachable())
      colon <- tokenizer.expect(ControlToken.Colon)
      value <- parseValue()
    } yield (key, value)

  def parseArray() =
    for {
      leftBracket <- tokenizer.expect(ControlToken.LeftBracket)
      items <- tokenizer
        .punctuated(ControlToken.Comma, ControlToken.RightBracket) {
          parseValue()
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

  def parseString() =
    for {
      stringToken <- tokenizer.tokenize(StringToken)
    } yield Json.ValueString(stringToken.content)

  def parseNull() =
    for {
      _ <- tokenizer.tokenize(NullToken)
    } yield Json.ValueNull

object Parser:
  def apply(target: String) =
    new Parser(
      new Tokenizer(
        new RowColIterator(target.linesIterator.toSeq.map(_.toSeq))
      )
    )
