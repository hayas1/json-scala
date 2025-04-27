class Parser(tokenizer: Tokenizer):
  def parseValue(): Either[Tokenizer.TokenError, Json] =
    tokenizer.lookAhead() match
      case Some(ControlToken.LeftBrace) => parseObject()
      case Some(ControlToken.Quote)     => parseString()
      case Some(ControlToken.Null)      => parseNull()
      case _                            => throw new NotImplementedError

  def parseObject() =
    for {
      leftBrace <- tokenizer.expect(ControlToken.LeftBrace)
      items <- tokenizer
        .punctuated(ControlToken.Comma, ControlToken.RightBrace) {
          parseObjectItem()
        }
    } yield Json.ValueObject(
      items
        .collect { case Right(item) => item } // TODO filtered?
        .foldLeft(Map.empty) { (map, item) => map + item }
    )

  def parseObjectItem() =
    for {
      keySource <- parseString()
      key <- keySource.asString.toRight(Tokenizer.Unreachable())
      colon <- tokenizer.expect(ControlToken.Colon)
      value <- parseValue()
    } yield (key, value)

  def parseString() =
    for {
      stringToken <- tokenizer.tokenizeString()
    } yield Json.ValueString(stringToken.content)

  def parseNull() =
    for {
      _ <- tokenizer.expect(ControlToken.Null)
    } yield Json.ValueNull

object Parser:
  def apply(target: String) =
    new Parser(Tokenizer(target))
