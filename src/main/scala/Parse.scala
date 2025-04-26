class Parser(tokenizer: Tokenizer):
  def parse_value(): Either[Tokenizer.TokenError, Json] =
    ControlToken(tokenizer.dropWhitespace().peek) match
      case Some(ControlToken.LeftBrace) => parse_object()
      case Some(ControlToken.Quote)     => parse_string()
      case _                            => Right(Json.ValueNull)

  def parse_object() =
    for {
      leftBrace <- tokenizer.expect(ControlToken.LeftBrace)
      items = tokenizer
        .takeUntil(ControlToken.RightBrace) { parse_object_item() }
        .collect { case Right(item) => item } // TODO filtered?
        .foldLeft(Map.empty[String, Json]) { (map, item) => map + item }
      rightBrace <- tokenizer.expect(ControlToken.RightBrace)
    } yield Json.ValueObject(items)

  def parse_object_item() =
    for {
      keySource <- parse_string()
      key <- keySource.asString.toRight(Tokenizer.Unreachable())
      colon <- tokenizer.expect(ControlToken.Colon)
      value <- parse_value()
    } yield (key, value)

  def parse_string() =
    for {
      quote <- tokenizer.expect(ControlToken.Quote)
      value = tokenizer
        .takeUntil(ControlToken.Quote) { tokenizer.nextChar() }
        .mkString
      endQuote <- tokenizer.expect(ControlToken.Quote)
    } yield Json.ValueString(value)

object Parser:
  def apply(target: String) =
    new Parser(Tokenizer(target))
