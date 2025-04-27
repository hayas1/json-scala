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
      objects <- items
        .foldLeft(
          Right(Map.empty): Either[Tokenizer.TokenError, Map[String, Json]]
        ) { (either_map, either_item) =>
          for {
            map <- either_map
            item <- either_item
          } yield map + item
        }
    } yield Json.ValueObject(objects)

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
      _ <- tokenizer.tokenizeNull()
    } yield Json.ValueNull

object Parser:
  def apply(target: String) =
    new Parser(Tokenizer(target))
