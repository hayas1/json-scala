class Parser(tokenizer: Tokenizer):
  def parse_value(): Either[Tokenizer.TokenError, Json] =
    tokenizer.lookAhead() match
      case Some(ControlToken.LeftBrace) => parse_object()
      case Some(ControlToken.Quote)     => parse_string()
      case _                            => Right(Json.ValueNull)

  def parse_object() =
    for {
      leftBrace <- tokenizer.expect(ControlToken.LeftBrace)
      items <- tokenizer
        .punctuated(ControlToken.Comma, ControlToken.RightBrace) {
          parse_object_item()
        }
    } yield Json.ValueObject(
      items
        .collect { case Right(item) => item } // TODO filtered?
        .foldLeft(Map.empty) { (map, item) => map + item }
    )

  def parse_object_item() =
    for {
      keySource <- parse_string()
      key <- keySource.asString.toRight(Tokenizer.Unreachable())
      colon <- tokenizer.expect(ControlToken.Colon)
      value <- parse_value()
    } yield (key, value)

  def parse_string() =
    for {
      startQuote <- tokenizer.expect(ControlToken.Quote)
      value <- tokenizer.tokenize_string_content()
      endQuote <- tokenizer.expect(ControlToken.Quote)
    } yield Json.ValueString(value)

object Parser:
  def apply(target: String) =
    new Parser(Tokenizer(target))
